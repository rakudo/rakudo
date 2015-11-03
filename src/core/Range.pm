my class X::Immutable { ... }
my class X::Range::InvalidArg { ... }

my class Range is Cool does Iterable does Positional {
    has $.min;
    has $.max;
    has int $!excludes-min;
    has int $!excludes-max;
    has int $!infinite;
    has int $!is-int;
    method is-lazy { self.infinite }

    # The order of "method new" declarations matters here, to ensure
    # appropriate candidate tiebreaking when mixed type arguments
    # are present (e.g., Range,Whatever or Real,Range).
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
        nqp::create(self).BUILD(-Inf,Inf,$excludes-min,$excludes-max,1);
    }
    multi method new(Whatever \min, \max, :$excludes-min, :$excludes-max) {
        nqp::create(self).BUILD(-Inf,max,$excludes-min,$excludes-max,1);
    }
    multi method new(\min, Whatever \max, :$excludes-min, :$excludes-max) {
        nqp::create(self).BUILD(min,Inf,$excludes-min,$excludes-max,1);
    }
    multi method new(Real \min, Real() $max, :$excludes-min, :$excludes-max) {
        nqp::create(self).BUILD(
          min,$max,$excludes-min,$excludes-max,$max == Inf || min == -Inf);
    }
    multi method new(List:D \min, \max, :$excludes-min, :$excludes-max) {
        nqp::create(self).BUILD(
          +min,
          nqp::istype(max,List) || nqp::istype(max,Match) ?? +max !! max,
          $excludes-min, $excludes-max, 0);
    }
    multi method new(Match:D \min, \max, :$excludes-min, :$excludes-max) {
        nqp::create(self).BUILD(
          +min,
          nqp::istype(max,List) || nqp::istype(max,Match) ?? +max !! max,
          $excludes-min, $excludes-max, 0);
    }
    multi method new(\min, \max, :$excludes-min, :$excludes-max!) {
        nqp::create(self).BUILD(min, max,$excludes-min,$excludes-max,0);
    }
    multi method new(\min, \max, :$excludes-min!, :$excludes-max) {
        nqp::create(self).BUILD(min,max,$excludes-min,$excludes-max,0);
    }
    multi method new(\min, \max) { nqp::create(self).BUILD(min,max,0,0,0) }

    submethod BUILD( $!min, $!max, \excludes-min, \excludes-max, \infinite) {
        $!excludes-min = excludes-min // 0;
        $!excludes-max = excludes-max // 0;
        $!infinite = infinite;
        $!is-int   = nqp::istype($!min,Int) && nqp::istype($!max,Int);
        self;
    }

    method excludes-min() { ?$!excludes-min }
    method excludes-max() { ?$!excludes-max }
    method infinite()     { ?$!infinite     }
    method is-int()       { ?$!is-int       }

    multi method WHICH (Range:D:) {
        self.^name
          ~ "|$!min"
          ~ ("^" if $!excludes-min)
          ~ '..'
          ~ ("^" if $!excludes-max)
          ~ $!max;
    }
    multi method EXISTS-POS(Range:D: int \pos) {
        pos < self.elems;
    }

    multi method EXISTS-POS(Range:D: Int \pos) {
        pos < self.elems;
    }

    method elems {
        $!is-int
          ?? 0 max $!max - $!excludes-max - $!min - $!excludes-min + 1
          !! $!infinite ?? Inf !! nextsame;
    }

    method iterator() {
        # can use native ints
        if $!is-int
          && !nqp::isbig_I(nqp::decont($!min))
          && !nqp::isbig_I(nqp::decont($!max)) {
            class :: does Iterator {
                has int $!i;
                has int $!n;

                method BUILD(\i,\n) { $!i = i - 1; $!n = n; self }
                method new(\i,\n)   { nqp::create(self).BUILD(i,n) }

                method pull-one() {
                    ( $!i = $!i + 1 ) <= $!n ?? $!i !! IterationEnd
                }
                method push-exactly($target, int $n) {
                    my int $left = $!n - $!i - 1;
                    if $n > $left {
                        $target.push(nqp::p6box_i($!i))
                          while ($!i = $!i + 1) <= $!n;
                       IterationEnd
                    }
                    else {
                        my int $end = $!i + 1 + $n;
                        $target.push(nqp::p6box_i($!i))
                          while ($!i = $!i + 1) < $end;
                        $!i = $!i - 1; # did one too many
                        $n
                    }
                }
                method push-all($target) {
                    my int $i = $!i;
                    my int $n = $!n;
                    $target.push(nqp::p6box_i($i)) while ($i = $i + 1) <= $n;
                    $!i = $i;
                    IterationEnd
                }
                method count-only() { nqp::p6box_i($!n - $!i + 1) }
                method sink-all()   { $!i = $!n; IterationEnd }
            }.new($!min + $!excludes-min, $!max - $!excludes-max)
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

                method BUILD(\i)  { $!i = i; self }
                method new(\i)    { nqp::create(self).BUILD(i) }
                method pull-one() { $!i++ }
                method is-lazy()  { True  }
            }.new($!min + $!excludes-min)
        }

        # if we have (simple) char range
        elsif nqp::istype($!min,Str) {
            my $min = $!excludes-min ?? $!min.succ !! $!min;
            $min after $!max
              ?? ().iterator
              !! $min.chars == 1 && nqp::istype($!max,Str) && $!max.chars == 1
                ?? class :: does Iterator {
                       has int $!i;
                       has int $!n;

                       method BUILD(\from,\end) {
                           $!i = nqp::ord(nqp::unbox_s(from)) - 1;
                           $!n = nqp::ord(nqp::unbox_s(end));
                           self
                       }
                       method new(\from,\end) {
                           nqp::create(self).BUILD(from,end)
                       }
                       method pull-one() {
                           ( $!i = $!i + 1 ) <= $!n
                             ?? nqp::chr($!i)
                             !! IterationEnd
                       }
                       method push-all($target) {
                           my int $i = $!i;
                           my int $n = $!n;
                           $target.push(nqp::chr($i)) while ($i = $i + 1) <= $n;
                           $!i = $i;
                           IterationEnd
                       }
                       method count-only() { nqp::p6box_i($!n - $!i + 1) }
                       method sink-all()   { $!i = $!n; IterationEnd }
                   }.new($min, $!excludes-max ?? $!max.pred !! $!max)
                !! SEQUENCE($min,$!max,:exclude_end($!excludes-max)).iterator
        }

        # General case according to spec
        else {
            class :: does Iterator {
                has $!i;
                has $!e;
                has int $!exclude;

                method BUILD(\i,\exclude,\e) {
                    $!i       = i;
                    $!exclude = exclude.Int;
                    $!e       = e;
                    self
                }
                method new(\i,\exclude,\e) {
                    nqp::create(self).BUILD(i,exclude,e)
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
                method push-all($target) {
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
                    IterationEnd
                }
                method count-only {
                    my Mu $i = $!i;
                    my Mu $e = $!e;
                    my int $found;
                    if $!exclude {
                        while $i before $e {
                            $found = $found + 1;
                            $i     = $i.succ;
                        }
                    }
                    else {
                        while not $i after $e {
                            $found = $found + 1;
                            $i     = $i.succ;
                        }
                    }
                    nqp::p6box_i($found)
                }
                method sink-all {
                    $!i = $!e;
                    IterationEnd
                }
            }.new($!excludes-min ?? $!min.succ !! $!min,$!excludes-max,$!max)
        }
    }
    multi method list(Range:D:) { List.from-iterator(self.iterator) }
    method flat(Range:D:) { Seq.new(self.iterator) }

    method bounds() { (nqp::decont($!min), nqp::decont($!max)) }
    method int-bounds() {
        $!is-int
          ?? ($!min + $!excludes-min, $!max - $!excludes-max)
          !! fail "Cannot determine integer bounds";
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

    multi method ACCEPTS(Range:D: Range \topic) {
        (topic.min > $!min
         || topic.min == $!min
            && !(!topic.excludes-min && $!excludes-min))
        &&
        (topic.max < $!max
         || topic.max == $!max
            && !(!topic.excludes-max && $!excludes-max))
    }

    multi method AT-POS(Range:D: int \pos) {
        self.list.AT-POS(pos);
    }
    multi method AT-POS(Range:D: Int:D \pos) {
        self.list.AT-POS(nqp::unbox_i(pos));
    }

    multi method perl(Range:D:) {
        $!is-int && $!min == 0 && !$!excludes-min && $!excludes-max
            ?? "^$!max"
            !! "{$!min.perl}{'^' if $!excludes-min}..{'^' if $!excludes-max}$!max.perl()"
    }

    proto method roll(|) { * }
    multi method roll(Range:D: Whatever) {
        if self.elems -> $elems {
            $!is-int
              ?? Seq.new(class :: does Iterator {
                    has int $!min;
                    has Int $!elems;
                    method BUILD(\min,\elems) {
                        $!min    = min;
                        $!elems := nqp::decont(elems);
                        self
                    }
                    method new(\b,\e) { nqp::create(self).BUILD(b,e) }
                    method pull-one() { $!min + nqp::rand_I($!elems, Int) }
                    method is-lazy()  { True }
                }.new($!min + $!excludes-min, $elems))
              !! self.list.roll(*)
        }
        else {
            Any xx *
        }
    }
    multi method roll(Range:D:) {
        if $!is-int {
            my $elems = $!max - $!excludes-max - $!min - $!excludes-min + 1;
            $elems > 0
              ?? $!min + $!excludes-min + nqp::rand_I(nqp::decont($elems),Int)
              !! Any
        }
        else {
            self.list.roll
        }
    }
    multi method roll(Int(Cool) $todo) {
        if self.elems -> $elems {
            $!is-int
              ?? Seq.new(class :: does Iterator {
                    has int $!min;
                    has Int $!elems;
                    has int $!todo;
                    method BUILD(\min,\elems,\todo) {
                        $!min    = min;
                        $!elems := nqp::decont(elems);
                        $!todo   = todo;
                        self
                    }
                    method new(\m,\e,\t) { nqp::create(self).BUILD(m,e,t) }
                    method pull-one() {
                        $!todo--
                          ?? $!min + nqp::rand_I($!elems, Int)
                          !! IterationEnd
                    }
                    method push-all($target) {
                        $target.push($!min + nqp::rand_I($!elems, Int))
                          while $!todo--;
                        IterationEnd
                    }
                }.new($!min + $!excludes-min,$elems,0 max $todo))
              !! self.list.roll($todo)
        }
        else {
            Any xx $todo
        }
    }

    proto method pick(|)        { * }
    multi method pick()          { self.roll };
    multi method pick(Whatever)  { self.list.pick(*) };
    multi method pick(Int(Cool) $todo) {
        if self.elems -> $elems {
            $!is-int && $elems > 3 * $todo # heuristic for sparse lookup
              ?? Seq.new(class :: does Iterator {
                    has int $!min;
                    has Int $!elems;
                    has int $!todo;
                    has $!seen;
                    method BUILD(\min,\elems,\todo) {
                        $!min    = min;
                        $!elems := nqp::decont(elems);
                        $!todo   = todo;
                        $!seen  := nqp::hash();
                        self
                    }
                    method new(\m,\e,\t) { nqp::create(self).BUILD(m,e,t) }
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
                    method push-all($target) {
                        my Int $value;
                        my str $key;
                        while $!todo {
                            $value = $!min + nqp::rand_I($!elems, Int);
                            $key   = nqp::tostr_I(nqp::decont($value));
                            unless nqp::existskey($!seen,$key) {
                                $target.push($value);
                                $!todo = $!todo - 1;
                                nqp::bindkey($!seen,$key,1);
                            }
                        }
                        IterationEnd
                    }
                }.new($!min + $!excludes-min,$elems,0 max $todo))
              !! self.list.pick($todo)
        }
        else {
            Any xx $todo
        }
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

    method clone-with-op(&op, $value) {
        my $min    = $!min [&op] $value;
        my $max    = $!max [&op] $value;
        my $is-int = nqp::istype($min,Int) && nqp::istype($max,Int);
        self.clone( :$min, :$max, :$is-int );
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
}

sub infix:<..>($min, $max) is pure {
    Range.new($min, $max)
}
sub infix:<^..>($min, $max) is pure {
    Range.new($min, $max, :excludes-min)
}
sub infix:<..^>($min, $max) is pure {
    Range.new($min, $max, :excludes-max)
}
sub infix:<^..^>($min, $max) is pure {
    Range.new($min, $max, :excludes-min, :excludes-max)
}
sub prefix:<^>($max) is pure {
    Range.new(0, $max.Numeric, :excludes-max)
}

multi sub infix:<eqv>(Range:D \a, Range:D \b) {
       a.min eqv b.min
    && a.max eqv b.max
    && a.excludes-min eqv b.excludes-min
    && a.excludes-max eqv b.excludes-max
}

multi sub infix:<+>(Range:D \a, Real:D \b) { a.clone-with-op(&[+], b) }
multi sub infix:<+>(Real:D \a, Range:D \b) { b.clone-with-op(&[+], a) }
multi sub infix:<->(Range:D \a, Real:D \b) { a.clone-with-op(&[-], b) }
multi sub infix:<*>(Range:D \a, Real:D \b) { a.clone-with-op(&[*], b) }
multi sub infix:<*>(Real:D \a, Range:D \b) { b.clone-with-op(&[*], a) }
multi sub infix:</>(Range:D \a, Real:D \b) { a.clone-with-op(&[/], b) }

# vim: ft=perl6 expandtab sw=4
