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

    method is-hidden(Backtrace::Frame:D:)  { $!code.?is-hidden-from-backtrace }
    method is-routine(Backtrace::Frame:D:) { nqp::istype($!code,Routine) }
    method is-setting(Backtrace::Frame:D:) {
        $!file.chars > 12 && substr($!file,*-12) eq 'CORE.setting'
    }
}

my class Backtrace is List {
    proto method new(|) {*}

    multi method new(Exception $e, Int $offset = 0) {
        self.new(nqp::backtrace(nqp::getattr(nqp::decont($e), Exception, '$!ex')), $offset);
    }

    multi method new() {
        try { die() };
        self.new($!, 2);
    }

    # note that backtraces are nqp::list()s, marshalled to us as Parcel
    multi method new(Parcel $bt, Int $offset = 0) {
        my $new = self.bless();
        for $offset .. $bt.elems - 1 {
            next unless defined $bt[$_]<sub>;
            my Mu $sub := nqp::getattr(nqp::decont($bt[$_]<sub>), ForeignCode, '$!do');
            next if nqp::isnull($sub);
            my $code;
            try {
                $code := nqp::getcodeobj($sub);
                $code := Any unless nqp::istype($code, Mu);
            };
            my $line     = $bt[$_]<annotations><line>;
            my $file     = $bt[$_]<annotations><file>;
            next unless $line && $file;
            # now *that's* an evil hack
            next if $file eq 'src/gen/BOOTSTRAP.nqp' ||
                    $file eq 'src/gen/m-BOOTSTRAP.nqp' ||
                    $file eq 'src\\gen\\BOOTSTRAP.nqp' ||
                    $file eq 'src\\gen\\m-BOOTSTRAP.nqp' ||
                    $file eq 'gen/jvm/stage2/QRegex.nqp' ||
                    $file eq 'gen/moar/stage2/QRegex.nqp';
            last if $file eq 'src/stage2/gen/NQPHLL.nqp' ||
                    $file eq 'src\\stage2\\gen\\NQPHLL.nqp' ||
                    $file eq 'gen/jvm/stage2/NQPHLL.nqp' ||
                    $file eq 'gen\\jvm\\stage2\\NQPHLL.nqp' ||
                    $file eq 'gen/moar/stage2/NQPHLL.nqp' ||
                    $file eq 'gen\\moar\\stage2\\NQPHLL.nqp';
            my $subname  = nqp::p6box_s(nqp::getcodename($sub));
            $subname = '<anon>' if substr($subname,0, 6) eq '_block';
            last if $subname eq 'handle-begin-time-exceptions';
            $new.push: Backtrace::Frame.new(
                :line($line.Int),
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
            my $cand = self.AT-POS($idx);
            next if $cand.is-hidden;          # hidden is never interesting
            next if $named && !$cand.subname; # only want named ones
            next if $noproto                  # no proto's please
              && $cand.code.?is_dispatcher;   #  if a dispatcher
            return $idx;
        }
        Int;
    }

    method outer-caller-idx(Backtrace:D: Int $startidx) {
        my %print;
        my $start   = self.AT-POS($startidx).code;
        return $startidx.list unless $start;
        my $current = $start.outer;
        my %outers;
        while $current.DEFINITE {
            %outers{$current.static_id} = $start;
            $current = $current.outer;
        }
        my @outers;
        loop (my Int $i = $startidx; $i < $.end; ++$i) {
            if self.AT-POS($i).code.DEFINITE && %outers{self.AT-POS($i).code.static_id}.DEFINITE {
                @outers.push: $i;
                return @outers if self.AT-POS($i).is-routine;
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
                          && self.AT-POS($i).is-setting;

                last unless $i.defined;
                my $prev = self.AT-POS($i);
                if $prev.is-routine {
                    @frames.push: $prev;
                } else {
                    my @outer_callers := self.outer-caller-idx($i);
                    my ($target_idx) = @outer_callers.keys.grep({self.AT-POS($i).code.^isa(Routine)});
                    $target_idx    ||= @outer_callers[0] || $i;
                    my $current = self.AT-POS($target_idx);
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

# vim: ft=perl6 expandtab sw=4
