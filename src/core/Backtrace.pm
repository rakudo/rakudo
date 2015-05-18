my class Exception { ... }

my class Backtrace { ... }

my $RAKUDO-VERBOSE-STACKFRAME;

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
        my $text = "  in {$s}$.subname at {$.file}:$.line\n";

        if $RAKUDO-VERBOSE-STACKFRAME -> $extra {
            my $io = $!file.IO;
            if $io.e {
                my @lines := $io.lines;
                my $from = max $!line - $extra, 1;
                my $to   = min $!line + $extra, +@lines;
                for $from..$to -> $line {
                    my $star = $line == $!line ?? '*' !! ' ';
                    $text ~= "$line.fmt('%5d')$star @lines[$line - 1]\n";
                }
                $text ~= "\n";
            }
        }
        $text;
    }

    method is-hidden(Backtrace::Frame:D:)  { $!code.?is-hidden-from-backtrace }
    method is-routine(Backtrace::Frame:D:) { nqp::istype($!code,Routine) }
    method is-setting(Backtrace::Frame:D:) { $!file.ends-with("CORE.setting") }
}

my class Backtrace {
    has Mu $!bt;
    has Mu $!frames;
    has Int $!bt-next;   # next bt index to vivify

    submethod BUILD(:$!bt, :$!bt-next) {
        $!frames := nqp::list;
        self;
    }

    proto method new(|) {*}
    multi method new(Mu $e, Int $offset = 0) {
        $e.^name eq 'BOOTException'
            ?? self.new(nqp::backtrace(nqp::decont($e)), $offset)
            !! self.new(nqp::backtrace(nqp::getattr(nqp::decont($e), Exception, '$!ex')), $offset);
    }

    multi method new(Int $offset = 0) {
        try { die() };
        self.new($!, 2 + $offset);
    }

    # note that backtraces are nqp::list()s, marshalled to us as Parcel
    multi method new(Parcel $bt, Int $bt-next = 0) {

        # only check for verbose stack frames once
        $RAKUDO-VERBOSE-STACKFRAME = +(%*ENV<RAKUDO_VERBOSE_STACKFRAME> // 0);

        nqp::create(self).BUILD(:$bt, :$bt-next);
    }

    method AT-POS($pos) {
        if nqp::atpos($!frames,$pos) -> $frame {
            return $frame;
        }

        my int $elems = $!bt.elems;
        my int $todo  = $pos - nqp::elems($!frames) + 1;

        while $!bt-next < $elems {

            my $frame := $!bt.AT-POS($!bt-next++);
            my $sub := $frame<sub>;
            next unless defined $sub;

            my Mu $do := nqp::getattr(nqp::decont($sub), ForeignCode, '$!do');
            next if nqp::isnull($do);

            my $annotations := $frame<annotations>;
            next unless $annotations;

            my $file := $annotations<file>;
            next unless $file;

            # now *that's* an evil hack
            next if $file.ends-with('BOOTSTRAP.nqp')
                 || $file.ends-with('QRegex.nqp');
            last if $file.ends-with('NQPHLL.nqp');

            my $line := $annotations<line>;
            next unless $line;

            my $name := nqp::p6box_s(nqp::getcodename($do));
            last if $name eq 'handle-begin-time-exceptions';

            my $code;
            try {
                $code := nqp::getcodeobj($do);
                $code := Any unless nqp::istype($code, Mu);
            };

            nqp::push($!frames,
              Backtrace::Frame.new(
                :$code,
                :$file,
                :line($line.Int),
                :subname($name.starts-with("_block") ?? '<anon>' !! $name),
              )
            );
            last unless $todo = $todo - 1;
        }

        # whatever is there (or not)
        nqp::atpos($!frames,$pos);
    }

    method next-interesting-index(Backtrace:D:
      Int $idx is copy = 0, :$named, :$noproto, :$setting) {
        ++$idx;

        while self.AT-POS($idx++) -> $cand {
            next if $cand.is-hidden;          # hidden is never interesting
            next if $named && !$cand.subname; # only want named ones
            next if $noproto                  # no proto's please
              && $cand.code.?is_dispatcher;   #  if a dispatcher
            next if !$setting                 # no settings please
              && $cand.is-setting;            #  and in setting
            return $idx - 1;
        }
        Int;
    }

    method outer-caller-idx(Backtrace:D: Int $startidx) {

        if self.AT-POS($startidx).code -> $start {
            my %outers;

            my $current = $start.outer;
            while $current.DEFINITE {
                %outers{$current.static_id} = $start;
                $current = $current.outer;
            }

            my @outers;
            my $i = $startidx;
            while self.AT-POS($i++) -> $cand {
                my $code = $cand.code;
                next unless $code.DEFINITE && %outers{$code.static_id}.DEFINITE;

                @outers.push: $i - 1;
                last if $cand.is-routine;
            }
            @outers;
        }

        else {
            $startidx.list;
        }
    }

    method nice(Backtrace:D: :$oneline) {
        my $setting = %*ENV<RAKUDO_BACKTRACE_SETTING>;
        try {
            my @frames;
            my Int $i = self.next-interesting-index(-1);
            while $i.defined {
                $i = self.next-interesting-index($i, :$setting) if $oneline;
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
                $i = self.next-interesting-index($i, :$setting);
            }
            CATCH {
                default {
                    return "<Internal error while creating backtrace: $_.message() $_.backtrace.full().\n"
                        ~ "Please report this as a bug (mail to rakudobug@perl.org)\n",
                        ~ "and re-run with the --ll-exception command line option\n"
                        ~ "to get more information about your error>";
                }
            }
            @frames.join;
        }
    }

    multi method Str(Backtrace:D:)  { self.nice }
    multi method flat(Backtrace:D:) { self.list }
    multi method map(Backtrace:D: $block) {
        my $pos = 0;
        gather while self.AT-POS($pos++) -> $cand {
            take $block($cand);
        }
    }
    multi method list(Backtrace:D:) {
        self.AT-POS(100);  # will stop when done, do we need more than 100???
        nqp::p6parcel($!frames,Mu);
    }

    method first-none-setting-line(Backtrace:D:) {
        self.first({ !.is-hidden && !.is-setting }).Str;
    }

    method concise(Backtrace:D:) {
        self.grep({ !.is-hidden && .is-routine && !.is-setting }).join;
    }

    method full(Backtrace:D:) { self.list.join }

    method summary(Backtrace:D:) {
        self.grep({ !.is-hidden && (.is-routine || !.is-setting) }).join;
    }
}

# vim: ft=perl6 expandtab sw=4
