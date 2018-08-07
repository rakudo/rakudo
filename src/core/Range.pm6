my class X::Immutable { ... }
my class X::Range::InvalidArg { ... }

my class Range is Cool does Iterable does Positional {
    has $.min;
    has $.max;
    has int $!excludes-min;
    has int $!excludes-max;
    has int $!infinite;
    has int $!is-int;

    method !SET-SELF(\min, \max, \excludes-min, \excludes-max, \infinite) {
        $!min := nqp::decont(min);
        $!max := nqp::decont(max);
        $!excludes-min = excludes-min // 0;
        $!excludes-max = excludes-max // 0;
        $!infinite = infinite;
        $!is-int   = nqp::istype($!min,Int) && nqp::istype($!max,Int);
        self
    }
    multi method is-lazy(Range:D:) { self.infinite }

    # The order of "method new" declarations matters here, to ensure
    # appropriate candidate tiebreaking when mixed type arguments
    # are present (e.g., Range,Whatever or Real,Range).
    proto method new(|) {*}
    multi method new(Range $min, \max, :$excludes-min, :$excludes-max) {
        X::Range::InvalidArg.new(:got($min)).throw;
    }
    multi method new(\min, Range $max, :$excludes-min, :$excludes-max) {
        X::Range::InvalidArg.new(:got($max)).throw;
    }
    multi method new(Seq \min, \max, :$excludes-min, :$excludes-max) {
        X::Range::InvalidArg.new(:got(Seq)).throw;
    }
    multi method new(\min , Seq \max, :$excludes-min, :$excludes-max) {
        X::Range::InvalidArg.new(:got(Seq)).throw;
    }
    multi method new(Complex \min, \max, :$excludes-min, :$excludes-max) {
        X::Range::InvalidArg.new(:got(min)).throw;
    }
    multi method new(\min , Complex \max, :$excludes-min, :$excludes-max) {
        X::Range::InvalidArg.new(:got(max)).throw;
    }
    multi method new(Whatever \min,Whatever \max,:$excludes-min,:$excludes-max){
        nqp::create(self)!SET-SELF(-Inf,Inf,$excludes-min,$excludes-max,1);
    }
    multi method new(Whatever \min, \max, :$excludes-min, :$excludes-max) {
        nqp::create(self)!SET-SELF(-Inf,max,$excludes-min,$excludes-max,1);
    }
    multi method new(\min, Whatever \max, :$excludes-min, :$excludes-max) {
        nqp::create(self)!SET-SELF(min,Inf,$excludes-min,$excludes-max,1);
    }
    multi method new(Real \min, Real() $max, :$excludes-min, :$excludes-max) {
        nqp::create(self)!SET-SELF(
          min,$max,$excludes-min,$excludes-max,
          $max == Inf || $max === NaN || min == -Inf || min === NaN
        );
    }
    multi method new(List:D \min, \max, :$excludes-min, :$excludes-max) {
        nqp::create(self)!SET-SELF(
          +min,
          nqp::istype(max,List) || nqp::istype(max,Match) ?? +max !! max,
          $excludes-min, $excludes-max, 0);
    }
    multi method new(Match:D \min, \max, :$excludes-min, :$excludes-max) {
        nqp::create(self)!SET-SELF(
          +min,
          nqp::istype(max,List) || nqp::istype(max,Match) ?? +max !! max,
          $excludes-min, $excludes-max, 0);
    }
    multi method new(\min, \max, :$excludes-min, :$excludes-max!) {
        nqp::create(self)!SET-SELF(min, max,$excludes-min,$excludes-max,0);
    }
    multi method new(\min, \max, :$excludes-min!, :$excludes-max) {
        nqp::create(self)!SET-SELF(min,max,$excludes-min,$excludes-max,0);
    }
    multi method new(\min, \max) { nqp::create(self)!SET-SELF(min,max,0,0,0) }

    method excludes-min() { nqp::p6bool($!excludes-min) }
    method excludes-max() { nqp::p6bool($!excludes-max) }
    method infinite()     { nqp::p6bool($!infinite)     }
    method is-int()       { nqp::p6bool($!is-int)       }

    method !IS-NATIVE-INT() {
        $!is-int && nqp::not_i(nqp::isbig_I($!min) || nqp::isbig_I($!max))
    }

    multi method WHICH (Range:D:) {
        (nqp::istype(self.WHAT,Range) ?? 'Range|' !! (self.^name ~ '|'))
          ~ $!min
          ~ ("^" if $!excludes-min)
          ~ '..'
          ~ ("^" if $!excludes-max)
          ~ $!max;
    }
    multi method EXISTS-POS(Range:D: int \pos) {
        0 <= pos < self.elems;
    }

    multi method EXISTS-POS(Range:D: Int \pos) {
        0 <= pos < self.elems;
    }

    method elems {
        $!is-int
          ?? 0 max $!max - $!excludes-max - $!min - $!excludes-min + 1
          !! $!infinite
            ?? Failure.new(X::Cannot::Lazy.new(:action<.elems>))
            !! nextsame
    }

    method iterator() {
        # can use native ints
        if nqp::istype($!min,Int) && nqp::not_i(nqp::isbig_I(nqp::decont($!min)))
          && ((nqp::istype($!max,Int) && nqp::not_i(nqp::isbig_I(nqp::decont($!max)))) || $!max == Inf) {
            Rakudo::Iterator.IntRange(
              $!min + $!excludes-min,
              $!max - $!excludes-max
            )
        }

        # doesn't make much sense, but there you go
        elsif $!min === -Inf {
            class :: does Iterator {
                method new()      { nqp::create(self) }
                method pull-one() { -Inf }
                method is-lazy()  { True  }
            }.new
        }

        # Also something quick and easy for 1..* style things
        elsif nqp::istype($!min, Numeric) && $!max === Inf {
            class :: does Iterator {
                has $!i;

                method !SET-SELF(\i)  { $!i = i - 1; self }
                method new(\i)    { nqp::create(self)!SET-SELF(i) }
                method pull-one() { ++$!i }
                method is-lazy()  { True  }
            }.new($!min + $!excludes-min)
        }

        # if we have (simple) char range
        elsif nqp::istype($!min,Str) {
            $!min after $!max
              ?? ().iterator
              !! $!min.chars == 1 && nqp::istype($!max,Str) && $!max.chars == 1
                ?? class :: does Iterator {
                       has int $!i;
                       has int $!n;

                       method !SET-SELF(\from,\end,\excludes-min,\excludes-max) {
                           $!i = nqp::ord(nqp::unbox_s(from))
                               - (excludes-min ?? 0 !! 1);
                           $!n = nqp::ord(nqp::unbox_s(end))
                               - (excludes-max ?? 1 !! 0);
                           self
                       }
                       method new(\from,\end,\excludes-min,\excludes-max) {
                           nqp::create(self)!SET-SELF(
                              from,end,excludes-min,excludes-max)
                       }
                       method pull-one() {
                           ( $!i = $!i + 1 ) <= $!n
                             ?? nqp::chr($!i)
                             !! IterationEnd
                       }
                       method push-all($target --> IterationEnd) {
                           my int $i = $!i;
                           my int $n = $!n;
                           $target.push(nqp::chr($i)) while ($i = $i + 1) <= $n;
                           $!i = $i;
                       }
                       method count-only() { nqp::p6box_i($!n - $!i) }
                       method bool-only() { nqp::p6bool(nqp::isgt_i($!n,$!i)) }
                       method sink-all(--> IterationEnd) { $!i = $!n }
                   }.new($!min, $!max, $!excludes-min, $!excludes-max)
                !! SEQUENCE(
                       ($!excludes-min ?? $!min.succ !! $!min),
                       $!max, :exclude_end($!excludes-max)
                   ).iterator
        }

        # General case according to spec
        else {
            class :: does Iterator {
                has $!i;
                has $!e;
                has int $!exclude;

                method !SET-SELF(\i,\exclude,\e) {
                    $!i       = i;
                    $!exclude = exclude.Int;
                    $!e       = e;
                    self
                }
                method new(\i,\exclude,\e) {
                    nqp::create(self)!SET-SELF(i,exclude,e)
                }

                method pull-one() {
                    if $!exclude ?? $!i before $!e !! not $!i after $!e {
                        my Mu $i = $!i;
                        $!i = $i.succ;
                        $i
                    }
                    else {
                        IterationEnd
                    }
                }
                method push-all($target --> IterationEnd) {
                    my Mu $i = $!i;
                    my Mu $e = $!e;
                    if $!exclude {
                        while $i before $e {
                            $target.push(nqp::clone($i));
                            $i = $i.succ;
                        }
                    }
                    else {
                        while not $i after $e {
                            $target.push(nqp::clone($i));
                            $i = $i.succ;
                        }
                    }
                    $!i = $e.succ;
                }
                method sink-all(--> IterationEnd) { $!i = $!e.succ }
            }.new($!excludes-min ?? $!min.succ !! $!min,$!excludes-max,$!max)
        }
    }
    multi method list(Range:D:) { List.from-iterator(self.iterator) }
    method flat(Range:D:) { Seq.new(self.iterator) }

    method !reverse-iterator() {
        # can use native ints
        if self!IS-NATIVE-INT() {
            class :: does Iterator {
                has int $!i;
                has int $!n;

                method !SET-SELF(\i,\n) { $!i = i + 1; $!n = n; self }
                method new(\i,\n)   { nqp::create(self)!SET-SELF(i,n) }

                method pull-one() {
                    ( $!i = $!i - 1 ) >= $!n ?? $!i !! IterationEnd
                }
                method push-all($target --> IterationEnd) {
                    my int $i = $!i;
                    my int $n = $!n;
                    $target.push(nqp::p6box_i($i)) while ($i = $i - 1) >= $n;
                    $!i = $i;
                }
                method count-only() { nqp::p6box_i($!i - $!n) }
                method bool-only() { nqp::p6bool(nqp::isgt_i($!i,$!n)) }
                method sink-all(--> IterationEnd)   { $!i = $!n }
            }.new($!max - $!excludes-max, $!min + $!excludes-min)
        }

        # doesn't make much sense, but there you go
        elsif $!max === -Inf {
            class :: does Iterator {
                method new()      { nqp::create(self) }
                method pull-one(--> Inf) { }
                method is-lazy(--> True) { }
            }.new
        }

        # Also something quick and easy for -Inf..42 style things
        elsif nqp::istype($!min, Numeric) && $!min === -Inf {
            class :: does Iterator {
                has $!i;

                method !SET-SELF(\i)  { $!i = i; self }
                method new(\i)    { nqp::create(self)!SET-SELF(i) }
                method pull-one() { $!i-- }
                method is-lazy()  { True  }
            }.new($!max - $!excludes-max)
        }

        # if we have (simple) char range
        elsif nqp::istype($!min,Str) {
            my $max = $!excludes-max ?? $!max.pred !! $!max;
            $max before $!min
              ?? ().iterator
              !! $max.chars == 1 && nqp::istype($!min,Str) && $!min.chars == 1
                ?? class :: does Iterator {
                       has int $!i;
                       has int $!n;

                       method !SET-SELF(\from,\end) {
                           $!i = nqp::ord(nqp::unbox_s(from)) + 1;
                           $!n = nqp::ord(nqp::unbox_s(end));
                           self
                       }
                       method new(\from,\end) {
                           nqp::create(self)!SET-SELF(from,end)
                       }
                       method pull-one() {
                           ( $!i = $!i - 1 ) >= $!n
                             ?? nqp::chr($!i)
                             !! IterationEnd
                       }
                       method push-all($target --> IterationEnd) {
                           my int $i = $!i;
                           my int $n = $!n;
                           $target.push(nqp::chr($i)) while ($i = $i - 1) >= $n;
                           $!i = $i;
                       }
                       method count-only() { nqp::p6box_i($!i - $!n) }
                       method bool-only() { nqp::p6bool(nqp::isgt_i($!i,$!n)) }
                       method sink-all(--> IterationEnd) { $!i = $!n }
                   }.new($max, $!excludes-min ?? $!min.succ !! $!min)
                !! SEQUENCE($max,$!min,:exclude_end($!excludes-min)).iterator
        }

        # General case according to spec
        else {
            class :: does Iterator {
                has $!i;
                has $!e;
                has int $!exclude;

                method !SET-SELF(\i,\exclude,\e) {
                    $!i       = i;
                    $!exclude = exclude.Int;
                    $!e       = e;
                    self
                }
                method new(\i,\exclude,\e) {
                    nqp::create(self)!SET-SELF(i,exclude,e)
                }

                method pull-one() {
                    if $!exclude ?? $!i after $!e !! not $!i before $!e {
                        my Mu $i = $!i;
                        $!i = $i.pred;
                        $i
                    }
                    else {
                        IterationEnd
                    }
                }
                method push-all($target --> IterationEnd) {
                    my Mu $i = $!i;
                    my Mu $e = $!e;
                    if $!exclude {
                        while $i after $e {
                            $target.push(nqp::clone($i));
                            $i = $i.pred;
                        }
                    }
                    else {
                        while not $i before $e {
                            $target.push(nqp::clone($i));
                            $i = $i.pred;
                        }
                    }
                }
                method sink-all(--> IterationEnd) { $!i = $!e }
            }.new($!excludes-max ?? $!max.pred !! $!max,$!excludes-min,$!min)
        }
    }
    method reverse(Range:D:) { Seq.new(self!reverse-iterator) }
    method first (|c) {
        if c<end> {
            my \res := self.reverse.first(|c, :!end);
            if c<k> and nqp::istype(res, Numeric) {
                self.elems - res - 1
            }
            elsif c<p> and nqp::istype(res, Pair) {
                Pair.new(self.elems - res.key - 1, res.value)
            }
            else {
                res
            }
        }
        else { nextsame };
    }

    method bounds() { ($!min, $!max) }
    proto method int-bounds(|) {*}
    multi method int-bounds($from is rw, $to is rw) {
        nqp::if(
          $!is-int,
          nqp::stmts(
            ($from = $!min + $!excludes-min),
            ($to   = $!max - $!excludes-max)
          ),
          nqp::if(
            nqp::istype($!min,Real)
              && $!min.floor == $!min
              && nqp::istype($!max,Real)
              && nqp::istype($!min.Int, Int)  # exclude NaN and Infs, who will fail() here
              && nqp::istype($!max.Int, Int),
            nqp::stmts(
              ($from = $!min.floor + $!excludes-min),
              ($to   = $!max.floor - ($!excludes-max && $!max.Int == $!max))
            ),
            Failure.new("Cannot determine integer bounds")
          )
        )
    }
    multi method int-bounds() {
        $!is-int
          ?? ($!min + $!excludes-min, $!max - $!excludes-max)
          !! nqp::istype($!min,Real) && $!min.floor == $!min && nqp::istype($!max,Real)
                && nqp::istype($!min.Int, Int) # exclude NaN and Infs, who will fail() here
                && nqp::istype($!max.Int, Int)
            ?? ($!min.floor + $!excludes-min, $!max.floor - ($!excludes-max && $!max.Int == $!max))
            !! Failure.new("Cannot determine integer bounds")
    }

    method fmt(|c) {
        self.list.fmt(|c)
    }

    multi method Str(Range:D:) {
        $!min === -Inf && $!max === Inf
          ?? "*{'^' if $!excludes-min}..{'^' if $!excludes-max}*"
          !! $!min === -Inf
            ?? "*{'^' if $!excludes-min}..{'^' if $!excludes-max}$!max"
            !! $!max === Inf
              ?? "{$!min}{'^' if $!excludes-min}..{'^' if $!excludes-max}*"
              !! self.list.Str
    }

    multi method ACCEPTS(Range:D: Mu \topic) {
        (topic cmp $!min) > -(!$!excludes-min)
          and (topic cmp $!max) < +(!$!excludes-max)
    }
    multi method ACCEPTS(Range:D: Cool:D \got) {
        $!is-int && nqp::istype(got,Int)
          ?? got >= $!min + $!excludes-min && got <= $!max - $!excludes-max
          !! ($!excludes-min ?? got after $!min !! not got before $!min)
               && ($!excludes-max ?? got before $!max !! not got after $!max)
    }
    multi method ACCEPTS(Range:D: Complex:D \got) {
        nqp::istype(($_ := got.Real), Failure) ?? False !! nextwith $_
    }
    multi method ACCEPTS(Range:D: Range \topic) {
        nqp::istype($!min, Numeric)
            ?? # RHS is a numeric range, use numeric comparators
                try {
                    (topic.min > $!min
                     || topic.min == $!min
                        && !(!topic.excludes-min && $!excludes-min))
                    &&
                    (topic.max < $!max
                     || topic.max == $!max
                        && !(!topic.excludes-max && $!excludes-max))
                } // False # don't explode on failures to coerce to numerics
            !! # RHS is a stringy range, use stringy comparators
                (topic.min gt $!min
                 || topic.min eq $!min
                    && !(!topic.excludes-min && $!excludes-min))
                &&
                (topic.max lt $!max
                 || topic.max eq $!max
                    && !(!topic.excludes-max && $!excludes-max))
    }

    method ASSIGN-POS(Range:D: |) { X::Assignment::RO.new(value => self).throw }

    multi method AT-POS(Range:D: int \pos) {
        $!is-int
            ?? self.EXISTS-POS(pos)
                ?? $!min + $!excludes-min + pos
                !! pos < 0
                    ?? Failure.new(X::OutOfRange.new(
                        :what($*INDEX // 'Index'), :got(pos), :range<0..^Inf>
                    )) !! Nil
            !! self.list.AT-POS(pos);
    }
    multi method AT-POS(Range:D: Int:D \pos) {
        $!is-int
            ?? self.EXISTS-POS(pos)
                ?? $!min + $!excludes-min + pos
                !! pos < 0
                    ?? Failure.new(X::OutOfRange.new(
                        :what($*INDEX // 'Index'), :got(pos), :range<0..^Inf>
                    )) !! Nil
            !! self.list.AT-POS(nqp::unbox_i(pos));
    }

    multi method perl(Range:D:) {
        $!is-int && $!min == 0 && !$!excludes-min && $!excludes-max
            ?? "^$!max"
            !! "{$!min.perl}{'^' if $!excludes-min}..{'^' if $!excludes-max}$!max.perl()"
    }

    proto method roll(|) {*}
    multi method roll(Range:D: Whatever) {
        if self.elems -> $elems {
            $!is-int
              ?? Seq.new(class :: does Iterator {
                    has $!min;
                    has $!elems;
                    method !SET-SELF(\min,\elems) {
                        $!min   := nqp::decont(min);
                        $!elems := nqp::decont(elems);
                        self
                    }
                    method new(\b,\e) { nqp::create(self)!SET-SELF(b,e) }
                    method pull-one() { $!min + nqp::rand_I($!elems, Int) }
                    method is-lazy(--> True) { }
                }.new($!min + $!excludes-min, $elems))
              !! self.list.roll(*)
        }
        else {
            EmptySeq
        }
    }
    multi method roll(Range:D:) {
        nqp::if(
          $!is-int,
          nqp::if(
            (my $elems := $!max - $!excludes-max - $!min - $!excludes-min+1) > 0,
            $!min + $!excludes-min + nqp::rand_I($elems,Int),
            Nil
          ),
          nqp::if(
            self.elems,
            self.list.roll,
            Nil
          )
        )
    }
    multi method roll(Int(Cool) $todo) {
        if self.elems -> $elems {
            $!is-int
              ?? Seq.new(class :: does Iterator {
                    has $!min;
                    has $!elems;
                    has Int $!todo;
                    method !SET-SELF(\min,\elems,\todo) {
                        $!min   := nqp::decont(min);
                        $!elems := nqp::decont(elems);
                        $!todo   = todo;
                        self
                    }
                    method new(\m,\e,\t) { nqp::create(self)!SET-SELF(m,e,t) }
                    method pull-one() {
                        $!todo--
                          ?? $!min + nqp::rand_I($!elems, Int)
                          !! IterationEnd
                    }
                    method push-all($target --> IterationEnd) {
                        $target.push($!min + nqp::rand_I($!elems, Int))
                          while $!todo--;
                    }
                }.new($!min + $!excludes-min,$elems,0 max $todo))
              !! self.list.roll($todo)
        }
        else {
            EmptySeq
        }
    }

    proto method pick(|)        {*}
    multi method pick()          { self.roll };
    multi method pick(Whatever)  {
        self.elems
          ?? self.list.pick(*)
          !! EmptySeq
    }
    multi method pick(Int(Cool) $todo) {
        if self.elems -> $elems {
            $!is-int && $elems > 3 * $todo # heuristic for sparse lookup
              ?? Seq.new(class :: does Iterator {
                    has $!min;
                    has $!elems;
                    has Int $!todo;
                    has $!seen;
                    method !SET-SELF(\min,\elems,\todo) {
                        $!min   := nqp::decont(min);
                        $!elems := nqp::decont(elems);
                        $!todo   = todo;
                        $!seen  := nqp::hash();
                        self
                    }
                    method new(\m,\e,\t) { nqp::create(self)!SET-SELF(m,e,t) }
                    method pull-one() {
                        my Int $value;
                        my str $key;
                        if $!todo {
                            repeat {
                                $value = $!min + nqp::rand_I($!elems, Int);
                                $key   = nqp::tostr_I(nqp::decont($value));
                            } while nqp::existskey($!seen,$key);
                            $!todo = $!todo - 1;
                            nqp::bindkey($!seen,$key,1);
                            $value
                        }
                        else {
                            IterationEnd
                        }
                    }
                    method push-all($target --> IterationEnd) {
                        my str $key;
                        while $!todo {
                            my Int $value = $!min + nqp::rand_I($!elems, Int);
                            $key = nqp::tostr_I(nqp::decont($value));
                            unless nqp::existskey($!seen,$key) {
                                $target.push($value);
                                $!todo = $!todo - 1;
                                nqp::bindkey($!seen,$key,1);
                            }
                        }
                    }
                }.new($!min + $!excludes-min,$elems,0 max $todo))
              !! self.list.pick($todo)
        }
        else {
            EmptySeq
        }
    }

    method Capture(Range:D:) {
        \( :$!min, :$!max,
           excludes-min => self.excludes-min,
           excludes-max => self.excludes-max,
           infinite     => self.infinite,
           is-int       => self.is-int)
    }

    multi method Numeric(Range:D:) {
        $!is-int
          ?? self.elems
          !! nqp::istype($!min,Numeric) && nqp::istype($!max,Numeric)
            ?? do {
                my $diff  = 0 max $!max - $!min - $!excludes-min;
                my $floor = $diff.floor;
                $floor + 1 - ($floor == $diff ?? $!excludes-max !! 0)
            }
            !! self.flat.elems
    }

    method push(|) is nodal {
        X::Immutable.new(:typename<Range>,:method<push>).throw
    }
    method append(|) is nodal {
        X::Immutable.new(:typename<Range>,:method<append>).throw
    }
    method unshift(|) is nodal {
        X::Immutable.new(:typename<Range>,:method<unshift>).throw
    }
    method prepend(|) is nodal {
        X::Immutable.new(:typename<Range>,:method<prepend>).throw
    }
    method shift(|) is nodal {
        X::Immutable.new(:typename<Range>,:method<shift>).throw
    }
    method pop(|) is nodal {
        X::Immutable.new(:typename<Range>, :method<pop>).throw
    }

    method sum() is nodal {
        self.int-bounds(my $start, my $stop)
          ?? ($start + $stop) * (0 max $stop - $start + 1) div 2
          !! $!min == -Inf
            ?? $!max == Inf
              ?? NaN
              !! -Inf
            !! $!max == Inf
              ?? Inf
              !! nextsame
    }

    method rand() {
        fail "Can only get a random value on Real values, did you mean .pick?"
          unless nqp::istype($!min,Real) && nqp::istype($!max,Real);
        fail "Can only get a random value from numeric values"
          if $!min === NaN || $!max === NaN;
        fail "Can not get a random value from an infinite range"
          if $!min === -Inf || $!max === Inf;

        my $range = $!max - $!min;
        fail "Can only get a random value if the range is positive"
          unless $range > 0;


        my $value = 0;
        if $!excludes-min || $!excludes-max {
            if $!excludes-min {
                if $!excludes-max {
                    $value = $range.rand
                        while $value+$!min == $!min || $value+$!min == $!max;
                }
                else {
                    $value = $range.rand while $value+$!min == $!min;
                }
            }
            else {  # $!excludes-max
                repeat {
                    $value = $range.rand
                } while $value+$!min == $!max;
            }
        }
        else {
            $value = $range.rand
        }
        $value + $!min;
    }

    method in-range($got, $what?) {
        self.ACCEPTS($got)
          || X::OutOfRange.new(:what($what // 'Value'),:got($got.perl),:range(self.gist)).throw
    }

    multi method minmax(Range:D:) {
        $!excludes-min || $!excludes-max
          ?? Failure.new("Cannot return minmax on Range with excluded ends")
          !! ($!min,$!max)
    }
}

proto sub infix:<..>($, $, *%) is pure {*}
multi sub infix:<..>($min, $max) { Range.new($min, $max) }

proto sub infix:<^..>($, $, *%) is pure {*}
multi sub infix:<^..>($min, $max) { Range.new($min, $max, :excludes-min) }

proto sub infix:<..^>($, $, *%) is pure {*}
multi sub infix:<..^>($min, $max) { Range.new($min, $max, :excludes-max) }

proto sub infix:<^..^>($, $, *%) is pure {*}
multi sub infix:<^..^>($min, $max) {
    Range.new($min, $max, :excludes-min, :excludes-max)
}

proto sub prefix:<^>($, *%) is pure {*}
multi sub prefix:<^>($max) { Range.new(0, $max.Numeric, :excludes-max) }

multi sub infix:<eqv>(Range:D \a, Range:D \b) {
    nqp::p6bool(
      nqp::eqaddr(a,b)
        || (nqp::eqaddr(a.WHAT,b.WHAT)
             && a.min eqv b.min
             && a.max eqv b.max
             && nqp::iseq_i(
               nqp::getattr_i(nqp::decont(a),Range,'$!excludes-min'),
               nqp::getattr_i(nqp::decont(b),Range,'$!excludes-min')
             )
             && nqp::iseq_i(
               nqp::getattr_i(nqp::decont(a),Range,'$!excludes-max'),
               nqp::getattr_i(nqp::decont(b),Range,'$!excludes-max')
             ))
    )
}

multi sub infix:<+>(Range:D \r, Real:D \v) {
    r.new: r.min + v, r.max + v, :excludes-min(r.excludes-min), :excludes-max(r.excludes-max)
}
multi sub infix:<+>(Real:D \v, Range:D \r) {
    r.new: v + r.min, v + r.max, :excludes-min(r.excludes-min), :excludes-max(r.excludes-max)
}
multi sub infix:<->(Range:D \r, Real:D \v) {
    r.new: r.min - v, r.max - v, :excludes-min(r.excludes-min), :excludes-max(r.excludes-max)
}
multi sub infix:<*>(Range:D \r, Real:D \v) {
    r.new: r.min * v, r.max * v, :excludes-min(r.excludes-min), :excludes-max(r.excludes-max)
}
multi sub infix:<*>(Real:D \v, Range:D \r) {
    r.new: v * r.min, v * r.max, :excludes-min(r.excludes-min), :excludes-max(r.excludes-max)
}
multi sub infix:</>(Range:D \r, Real:D \v) {
    r.new: r.min / v, r.max / v, :excludes-min(r.excludes-min), :excludes-max(r.excludes-max)
}

multi sub infix:<cmp>(Range:D \a, Range:D \b --> Order:D) {
    a.min cmp b.min || a.excludes-min cmp b.excludes-min || a.max cmp b.max || b.excludes-max cmp a.excludes-max
}
multi sub infix:<cmp>(Num(Real) \a, Range:D \b --> Order:D) { (a..a) cmp b }
multi sub infix:<cmp>(Range:D \a, Num(Real) \b --> Order:D) { a cmp (b..b) }

multi sub infix:<cmp>(Positional \a, Range:D \b --> Order:D) { a cmp b.list }
multi sub infix:<cmp>(Range:D \a, Positional \b --> Order:D) { a.list cmp b }

# vim: ft=perl6 expandtab sw=4
