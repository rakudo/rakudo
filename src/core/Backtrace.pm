my class Exception { ... }

my class Backtrace { ... }

my class Backtrace::Frame {
    has Str $.file;
    has Int $.line;
    has Mu  $.code;
    has Str $.subname;

    method subtype(Backtrace::Frame:D:) {
        my $s = $!code.^name.lc.split('+', 2)[0];
        $s eq 'mu' ?? '' !! $s;
    }

    method package(Backtrace::Frame:D:) {
        $.code.package;
    }

    multi method Str(Backtrace::Frame:D:) {
        my $s = self.subtype;
        $s ~= ' ' if $s.chars;
        "  in {$s}$.subname at {$.file}:$.line\n"
    }

    method is-hidden(Backtrace::Frame:D:)  { $!code.?is_hidden_from_backtrace }
    method is-routine(Backtrace::Frame:D:) { $!code ~~ Routine }
    method is-setting(Backtrace::Frame:D:) {
        $!file.chars > 12 && $!file.substr(*-12) eq 'CORE.setting'
    }
}

my class Backtrace is List {
    proto method new(|) {*}

    multi method new(Exception $e, Int $offset = 0) {
#?if parrot
        self.new(nqp::getattr(nqp::decont($e), Exception, '$!ex').backtrace, $offset);
#?endif
#?if jvm
        self.new(nqp::backtrace(nqp::getattr(nqp::decont($e), Exception, '$!ex')), $offset);
#?endif
    }

    multi method new() {
        try { die() };
        self.new($!, 3);
    }

    # note that parrot backtraces are RPAs, marshalled to us as Parcel
    multi method new(Parcel $bt, Int $offset = 0) {
        my $new = self.bless();
        for $offset .. $bt.elems - 1 {
            next unless defined $bt[$_]<sub>;
            my Mu $sub := nqp::getattr(nqp::decont($bt[$_]<sub>), ForeignCode, '$!do');
            next if nqp::isnull($sub);
            my $code;
            try {
                $code = nqp::getcodeobj($sub);
            };
            my $line     = $bt[$_]<annotations><line>;
            my $file     = $bt[$_]<annotations><file>;
            next unless $line && $file;
            # now *that's* an evil hack
            next if $file eq 'src/gen/BOOTSTRAP.nqp' ||
                    $file eq 'src\\gen\\BOOTSTRAP.nqp';
            last if $file eq 'src/stage2/gen/NQPHLL.nqp' ||
                    $file eq 'src\\stage2\\gen\\NQPHLL.nqp' ||
                    $file eq 'gen/parrot/stage2/NQPHLL.nqp' ||
                    $file eq 'gen\\parrot\\stage2\\NQPHLL.nqp' ||
                    $file eq 'gen/jvm/stage2/NQPHLL.nqp' ||
                    $file eq 'gen\\jvm\\stage2\\NQPHLL.nqp';
                    # XXX extend for moar
            my $subname  = nqp::p6box_s(nqp::getcodename($sub));
            $subname = '<anon>' if $subname.substr(0, 6) eq '_block';
            $new.push: Backtrace::Frame.new(
                :$line,
                :$file,
                :$subname,
                :$code,
            );
        }
        $new;
    }

    method next-interesting-index(Backtrace:D:
      Int $idx is copy = 0, :$named, :$noproto) {
        ++$idx;
        # NOTE: the < $.end looks like an off-by-one error
        # but it turns out that a simple   perl6 -e 'die "foo"'
        # has two bt frames from the mainline; so it's OK to never
        # consider the last one
        loop (; $idx < self.end; ++$idx) {
            my $cand = self.at_pos($idx);
            next if $cand.is-hidden;          # hidden is never interesting
            next if $named && !$cand.subname; # only want named ones
            next if $noproto                  # no proto's please
              && $cand.code.?is_dispatcher;   #  if a dispatcher
            return $idx;
        }
        Int;
    }

    method outer-caller-idx(Backtrace:D: Int $startidx is copy) {
        my %print;
        my $start   = self.at_pos($startidx).code;
        return $startidx.list unless $start;
        my $current = $start.outer;
        my %outers;
        while $current.DEFINITE {
            %outers{$current.static_id} = $start;
            $current = $current.outer;
        }
        my @outers;
        loop (my Int $i = $startidx; $i < $.end; ++$i) {
            if self.at_pos($i).code.DEFINITE && %outers{self.at_pos($i).code.static_id}.DEFINITE {
                @outers.push: $i;
                return @outers if self.at_pos($i).is-routine;
            }
        }

        return @outers;
    }

    method nice(Backtrace:D: :$oneline) {
        try {
            my @frames;
            my Int $i = self.next-interesting-index(-1);
            while $i.defined {
                $i = self.next-interesting-index($i)
                    while $oneline && $i.defined
                          && self.at_pos($i).is-setting;

                last unless $i.defined;
                my $prev = self.at_pos($i);
                if $prev.is-routine {
                    @frames.push: $prev;
                } else {
                    my @outer_callers := self.outer-caller-idx($i);
                    my ($target_idx) = @outer_callers.keys.grep({self.at_pos($i).code.^isa(Routine)});
                    $target_idx    ||= @outer_callers[0] || $i;
                    my $current = self.at_pos($target_idx);
                    @frames.push: $current.clone(line => $prev.line);
                    $i = $target_idx;
                }
                last if $oneline;
                $i = self.next-interesting-index($i);
            }
            return @frames.join;
            CATCH {
                default {
                    return "<Internal error while creating backtrace: $_.message() $_.backtrace.full().\n"
                        ~ "Please report this as a bug (mail to rakudobug@perl.org)\n", 
                        ~ "and re-run with the --ll-exception command line option\n"
                        ~ "to get more information about your error>";
                }
            }
        }
    }


    method concise(Backtrace:D:) {
        self.grep({ !.is-hidden && .is-routine && !.is-setting }).join
    }

    multi method Str(Backtrace:D:) {
        self.nice;
    }

    method full(Backtrace:D:) {
        self.join
    }

    method summary(Backtrace:D:) {
        self.grep({ !.is-hidden && (.is-routine || !.is-setting )}).join
    }
}
