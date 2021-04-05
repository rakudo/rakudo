my class Exception { ... }

my class Backtrace { ... }
my class CompUnit::RepositoryRegistry is repr('Uninstantiable') { ... }

my class Backtrace::Frame {
    has Str $.file;
    has Int $.line;
    has Mu  $.code;
    has Str $.subname;

    method !SET-SELF($!file,$!line,\code,$!subname) {
        $!code := code;
        self
    }
    multi method new(Backtrace::Frame: \file,\line,\code,\subname) {
        nqp::create(self)!SET-SELF(file,line,code,subname)
    }
    multi method new(Backtrace::Frame: |c) {
        self.bless(|c)
    }

    method subtype(Backtrace::Frame:D:) {
        my $s = $!code.^name.lc.split('+', 2).cache[0];
        $s eq 'mu' ?? '' !! $s;
    }

    method package(Backtrace::Frame:D:) {
        $.code.package;
    }

    multi method Str(Backtrace::Frame:D:) {
        my $s = self.subtype;
        $s ~= ' ' if $s.chars;
        my $text = "  in {$s}$.subname at {$.file} line $.line\n";

        if Backtrace.RAKUDO_VERBOSE_STACKFRAME -> $extra {
            my $io = $!file.IO;
            if $io.e {
                my @lines = $io.lines;
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

    method is-hidden(Backtrace::Frame:D:) {
        nqp::can($!code,"is-hidden-from-backtrace")
          ?? $!code.is-hidden-from-backtrace
          !! False
    }
    method is-routine(Backtrace::Frame:D:) {
        nqp::hllbool(nqp::istype($!code,Routine))
    }
    method is-setting(Backtrace::Frame:D:) {
        $!file.starts-with("SETTING::")
#?if jvm
          || $!file ~~ / "CORE." \w+ ".setting" $ /
#?endif
#?if !jvm
          || $!file ~~ / "CORE." \w+ ".setting.{ Rakudo::Internals.PRECOMP-EXT }" $ /
#?endif
          || $!file.ends-with(".nqp")
    }
}

my class Backtrace {
    has Mu $!bt;
    has Mu $!frames;
    has Int $!bt-next;   # next bt index to vivify

    my $RAKUDO_VERBOSE_STACKFRAME := nqp::null;
    method RAKUDO_VERBOSE_STACKFRAME() is implementation-detail {
        nqp::ifnull(
          $RAKUDO_VERBOSE_STACKFRAME,
          $RAKUDO_VERBOSE_STACKFRAME :=
            (%*ENV<RAKUDO_VERBOSE_STACKFRAME> // 0).Int
        )
    }

    method !SET-SELF($!bt,$!bt-next) {
        $!frames := nqp::list;
        self
    }
    multi method new() {
#?if !js
        nqp::create(self)!SET-SELF(
          nqp::backtrace(nqp::null),
          0)
#?endif
#?if js
        try X::AdHoc.new(:payload("Died")).throw;
        nqp::create(self)!SET-SELF(
          nqp::backtrace(nqp::getattr(nqp::decont($!),Exception,'$!ex')),
          1)
#?endif
    }
    multi method new(Int:D $offset) {
#?if !js
        nqp::create(self)!SET-SELF(
          nqp::backtrace(nqp::null),
          $offset)
#?endif
#?if js
        try X::AdHoc.new(:payload("Died")).throw;
        nqp::create(self)!SET-SELF(
          nqp::backtrace(nqp::getattr(nqp::decont($!),Exception,'$!ex')),
          1 + $offset)
#?endif
    }
    multi method new(Exception:D \ex) {
        nqp::create(self)!SET-SELF(
          nqp::backtrace(nqp::getattr(nqp::decont(ex),Exception,'$!ex')),
          0)
    }
    multi method new(Mu \ex) {
        # assume BOOTException
        nqp::create(self)!SET-SELF(nqp::backtrace(nqp::decont(ex)),0)
    }
    multi method new(Exception:D \ex, Int:D $offset) {
        nqp::create(self)!SET-SELF(
          nqp::backtrace(nqp::getattr(nqp::decont(ex),Exception,'$!ex')),
          $offset)
    }
    multi method new(Mu \ex, Int:D $offset) {
        # assume BOOTException
        nqp::create(self)!SET-SELF(nqp::backtrace(nqp::decont(ex)),$offset)
    }
    # note that backtraces are nqp::list()s, marshalled to us as a List
    multi method new(List:D $bt) {
        nqp::create(self)!SET-SELF($bt,0)
    }
    multi method new(List:D $bt, Int:D $offset) {
        nqp::create(self)!SET-SELF($bt,$offset)
    }

    method AT-POS($pos) {
        return nqp::atpos($!frames,$pos) if nqp::existspos($!frames,$pos);

        my int $elems = $!bt.elems;
        return Nil if $!bt-next >= $elems; # bt-next can init > elems

        my int $todo = $pos - nqp::elems($!frames) + 1;
        return Nil if $todo < 1; # in case absurd $pos passed
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

            if CompUnit::RepositoryRegistry.file-for-spec($file) -> $path {
                $file := $path.absolute;
            }

            next if $file.ends-with('BOOTSTRAP.nqp')
                 || $file.ends-with('QRegex.nqp')
                 || $file.ends-with('Perl6/Ops.nqp');
            if $file.ends-with('NQPHLL.nqp') || $file.ends-with('NQPHLL.moarvm') {
                # This could mean we're at the end of the interesting backtrace,
                # or it could mean that we're in something like sprintf (which
                # uses an NQP grammar to parse the format string).
                while $!bt-next < $elems {
                    my $frame := $!bt.AT-POS($!bt-next++);
                    my $annotations := $frame<annotations>;
                    next unless $annotations;
                    my $file := $annotations<file>;
                    next unless $file;
                    if $file.starts-with('SETTING::') {
                        $!bt-next--; # re-visit this frame
                        last;
                    }
                }
                next;
            }

            my $line := $annotations<line>;
            next unless $line;

            my $name := nqp::p6box_s(nqp::getcodename($do));
            if $name eq 'handle-begin-time-exceptions' {
                $!bt-next = $elems;
                last;
            }

            my $code;
            try {
                $code := nqp::getcodeobj($do);
                $code := Any unless nqp::istype($code, Mu);
            };

            nqp::push($!frames,
              Backtrace::Frame.new(
                $file,
                $line.Int,
                $code,
                $name.starts-with("_block") ?? '<anon>' !! $name,
              )
            );
            last unless $todo = $todo - 1;
        }

        # found something
        if nqp::existspos($!frames,$pos) {
            nqp::atpos($!frames,$pos);
        }

        # we've reached the end, don't show the last <unit-outer> if there is one
        else {
            nqp::pop($!frames) if $!frames;
            Nil;
        }
    }

    method next-interesting-index(Backtrace:D:
      Int $idx is copy = 0, :$named, :$noproto, :$setting) {
        ++$idx;

        while self.AT-POS($idx++) -> $cand {
            next if $cand.is-hidden;          # hidden is never interesting
            next if $noproto                  # no proto's please
              && nqp::can($cand,"is_dispatcher")
              && $cand.code.is_dispatcher;    #  if a dispatcher
            next if !$setting                 # no settings please
              && $cand.is-setting;            #  and in setting

            my $n := $cand.subname;
            next if $named && !$n;            # only want named ones and no name
            next if $n eq '<unit-outer>';     # outer calling context

            return $idx - 1;
        }
        Nil;
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
                    my $target_idx = @outer_callers.keys.grep({self.AT-POS($i).code.^isa(Routine)})[0];
                    $target_idx    ||= @outer_callers[0] || $i;
                    my $current = self.AT-POS($target_idx);
                    @frames.append: $current.clone(line => $prev.line);
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

    multi method gist(Backtrace:D:) {
        my $els := +self.list;
        'Backtrace(' ~ $els ~ ' frame' ~ 's' x ($els != 1) ~ ')'
    }
    multi method Str(Backtrace:D:)  { self.nice }
    multi method flat(Backtrace:D:) { self.list }
    multi method map(Backtrace:D: &block) {
        my $pos = 0;
        gather while self.AT-POS($pos++) -> $cand {
            take block($cand);
        }
    }
    multi method first(Backtrace:D: Mu $test) {
        my $pos = 0;
        while self.AT-POS($pos++) -> $cand {
            return-rw $cand if $cand ~~ $test;
        }
        Nil;
    }
    multi method list(Backtrace:D:) {
        self.AT-POS(1_000_000);  # will stop when no more frames to be found
        nqp::p6bindattrinvres(nqp::create(List), List, '$!reified', $!frames)
    }

    method first-none-setting-line(Backtrace:D:) {
        (self.first({ !.is-hidden && !.is-setting }) // "\n").Str;
    }

    method concise(Backtrace:D:) {
        (self.grep({ !.is-hidden && .is-routine && !.is-setting }) // "\n").join;
    }

    method full(Backtrace:D:) { self.list.join }

    method summary(Backtrace:D:) {
        (self.grep({ !.is-hidden && (.is-routine || !.is-setting)}) // "\n").join;
    }

    method is-runtime (Backtrace:D:) {
        my $bt = $!bt;
        for $bt.keys {
            my $p6sub := $bt[$_]<sub>;
            if nqp::istype($p6sub, ForeignCode) {
                try {
                    my Mu $sub := nqp::getattr(nqp::decont($p6sub), ForeignCode, '$!do');
                    my str $name = nqp::getcodename($sub);
                    return True if nqp::iseq_s($name, 'THREAD-ENTRY');
                    return True if nqp::iseq_s($name, 'eval');
                    return True if nqp::iseq_s($name, 'print_control');
                    return False if nqp::iseq_s($name, 'compile');
                }
            }
        }
        False;
    }

}

# vim: expandtab shiftwidth=4
