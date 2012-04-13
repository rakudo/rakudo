my class Exception { ... }


my class Backtrace is List {
    class Frame {
        has Str $.file;
        has Int $.line;
        has Mu  $.code;
        has Str $.subname;

        method subtype {
            my $s = $!code.^name.lc.split('+', 2)[0];
            $s eq 'mu' ?? '' !! $s;
        }

        multi method Str(Backtrace::Frame:D:) {
            my $s = $.subtype;
            $s ~= ' ' if $s.chars;
            "  in {$s}$.subname at {$.file}:$.line\n"
        }

        method is-hidden  { $!code.?is_hidden_from_backtrace }
        method is-routine { $!code ~~ Routine }
        method is-setting { $!file eq 'src/gen/CORE.setting' }
    }
    proto method new(|$) {*}

    multi method new(Exception $e, Int $offset = 0) {
        self.new(nqp::getattr(nqp::p6decont($e), Exception, '$!ex').backtrace, $offset);
    }

    multi method new() {
        try { die() };
        self.new($!, 3);
    }

    # note that parrot backtraces are RPAs, marshalled to us as Parcel
    multi method new(Parcel $bt, Int $offset = 0) {
        my $new = self.bless(*);
        for $offset .. $bt.elems - 1 {
            next if pir::isnull($bt[$_]<sub>);
            my $code;
            try {
                $code = pir::perl6_code_object_from_parrot_sub__PP($bt[$_]<sub>);
            };
            my $line     = $bt[$_]<annotations><line>;
            my $file     = $bt[$_]<annotations><file>;
            next unless $line && $file;
            # now *that's* an evil hack
            last if $file eq 'src/stage2/gen/NQPHLL.pm' ||
                    $file eq 'src\\stage2\\gen\\NQPHLL.pm';
            my $subname  = nqp::p6box_s($bt[$_]<sub>);
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

    method next-interesting-index(Int $idx is copy = 0) {
        ++$idx;
        # NOTE: the < $.end looks like an off-by-one error
        # but it turns out that a simple   perl6 -e 'die "foo"'
        # has two bt frames from the mainline; so it's OK to never
        # consider the last one
        loop (; $idx < $.end; ++$idx) {
            my $cand = $.at_pos($idx);
            return $idx unless $cand.is-hidden;
        }
        Int;
    }

    method outer-caller-idx(Int $startidx is copy) {
        my %print;
        my $start   = $.at_pos($startidx).code;
        return $startidx.list unless $start;
        my $current = $start.outer;
        my %outers;
        while $current {
            %outers{$current.static_id} = $start;
            $current = $current.outer;
        }
        my @outers;
        loop (my Int $i = $startidx; $i < $.end; ++$i) {
            if $.at_pos($i).code && %outers{%.at_pos($i).code.static_id} {
                @outers.push: $i;
                return @outers if $.at_pos($i).is-routine;
            }
        }

        return @outers;
    }

    method nice(:$oneline) {
        try {
            my @frames;
            my Int $i = self.next-interesting-index(-1);
            while $i.defined {
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
                    return "<Internal error while creating backtrace: $!.message().\n"
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
