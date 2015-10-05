my class X::Range::InvalidArg { ... }

my class Range is Cool does Iterable does Positional {
    has $.min;
    has $.max;
    has $.excludes-min;
    has $.excludes-max;
    has $.infinite;
    method is-lazy { self.infinite }

    # The order of "method new" declarations matters here, to ensure
    # appropriate candidate tiebreaking when mixed type arguments
    # are present (e.g., Range,Whatever or Real,Range).
    multi method new(Range $min, $max, :$excludes-min, :$excludes-max) {
        X::Range::InvalidArg.new(:got($min)).throw;
    }
    multi method new($min, Range $max, :$excludes-min, :$excludes-max) {
        X::Range::InvalidArg.new(:got($max)).throw;
    }
    multi method new(Seq $min, $max, :$excludes-min, :$excludes-max) {
        X::Range::InvalidArg.new(:got(Seq)).throw;
    }
    multi method new($min, Seq $max, :$excludes-min, :$excludes-max) {
        X::Range::InvalidArg.new(:got(Seq)).throw;
    }
    multi method new(Whatever $min, Whatever $max, :$excludes-min, :$excludes-max) {
        nqp::create(self).BUILD(-Inf, Inf, $excludes-min, $excludes-max, True);
    }
    multi method new(Whatever $min, $max, :$excludes-min, :$excludes-max) {
        nqp::create(self).BUILD(-Inf, $max, $excludes-min, $excludes-max, True);
    }
    multi method new($min, Whatever $max, :$excludes-min, :$excludes-max) {
        nqp::create(self).BUILD($min, Inf, $excludes-min, $excludes-max, True);
    }
    multi method new(Real $min, Real() $max, :$excludes-min, :$excludes-max) {
        nqp::create(self).BUILD(
          $min,
          $max,
          $excludes-min,
          $excludes-max,
          $max == Inf || $min == -Inf,
        );
    }
    multi method new($min is copy, $max is copy, :$excludes-min, :$excludes-max) {
        $min = +$min
          if nqp::istype($min,List) || nqp::istype($min,Match);
        $max = +$max
          if nqp::istype($max,List) || nqp::istype($max,Match);
        nqp::create(self).BUILD($min, $max, $excludes-min, $excludes-max);
    }

    submethod BUILD(
      $!min,
      $!max,
      Bool() $!excludes-min,
      Bool() $!excludes-max,
      Bool   $!infinite = False,
    ) {
        self;
    }

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
        return Inf if $!min === -Inf || $!max === Inf;
        if nqp::istype($!min, Int) && nqp::istype($!max, Int) {
            my Int $least =
              $!excludes-min ?? $!min + 1 !! $!min;
            return 1 + ($!excludes-max ?? $!max.Int - 1 !! $!max.Int) - $least;
        }
        nextsame;
    }

    method iterator() {
        # Obtain starting value.
        my $value = $!excludes-min ?? $!min.succ !! $!min;

        # Iterating a Str range delegates to iterating a sequence.
        if nqp::istype($value, Str) {
            $value after $!max
                ?? ().iterator
                !! SEQUENCE($value, $!max, :exclude_end($!excludes-max)).iterator
        }

        # If the value and the maximum are both integers and fit in a native
        # int, we have a really cheap approach.
        elsif nqp::istype($value, Int) && nqp::istype($!max, Int) &&
              !nqp::isbig_I(nqp::decont($value)) && !nqp::isbig_I(nqp::decont($!max)) {
            class :: does Iterator {
                has int $!i;
                has int $!n;

                method BUILD(int $i, int $n) { $!i = $i - 1; $!n = $n; self }
                method new(\i,\n) { nqp::create(self).BUILD(i,n) }

                method pull-one() {
                    ( $!i = $!i + 1 ) <= $!n ?? $!i !! IterationEnd
                }
                method push-exactly($target, int $n) {
                    my int $i;
                    $target.push(nqp::p6box_i($!i))
                      while (  $i =  $i + 1 ) <= $n   # not done all requested
                        &&  ( $!i = $!i + 1 ) <= $!n; # value in range
                    $!i < $!n ?? $i - 1 !! IterationEnd
                }
                method push-all($target) {
                    $target.push(nqp::p6box_i($!i))
                      while ( $!i = $!i + 1 ) <= $!n;
                    IterationEnd
                }
                method sink-all() {
                    $!i = $!n;
                    IterationEnd
                }
            }.new($value, $!excludes-max ?? $!max - 1 !! $!max)
        }

        # Also something quick and easy for 1..* style things.
        elsif nqp::istype($value, Numeric) && $!max === Inf {
            class :: does Iterator {
                has $!i;

                method new($i is copy) {
                    my \iter = self.CREATE;
                    nqp::bindattr(iter, self, '$!i', $i);
                    iter
                }

                method pull-one() {
                    $!i++
                }

                method is-lazy() { True }
            }.new($value)
        }

        # General case according to spec
        else {
            class :: does Iterator {
                has $!i;
                has $!e;
                has $!exclude;

                method new($i is copy, $exclude is copy, $e is copy) {
                    my \iter = self.CREATE;
                    nqp::bindattr(iter, self, '$!i', $i);
                    nqp::bindattr(iter, self, '$!e', $e);
                    nqp::bindattr(iter, self, '$!exclude', $exclude);
                    iter
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
            }.new($value, $!excludes-max, $!max)
        }
    }
    multi method list(Range:D:) { List.from-iterator(self.iterator) }
    method flat(Range:D:) { Seq.new(self.iterator) }

    method bounds()   { (nqp::decont($!min), nqp::decont($!max)) }

    method fmt(|c) {
        self.list.fmt(|c)
    }

    multi method Str(Range:D:) { self.list.Str }

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
        $.min.perl
          ~ ('^' if $.excludes-min)
          ~ '..'
          ~ ('^' if $.excludes-max)
          ~ $.max.perl
    }

    proto method roll(|) { * }
    multi method roll(Range:D: Whatever) {
        gather loop { take self.roll }
    }
    multi method roll(Range:D:) {
        return self.list.roll
          unless nqp::istype($!min, Int) && nqp::istype($!max, Numeric);

        my Int $least =
          $!excludes-min ?? $!min + 1 !! $!min;
        my Int $elems =
          1 + ($!excludes-max ?? $!max.Int - 1 !! $!max.Int) - $least;
        $elems ?? ($least + nqp::rand_I(nqp::decont($elems), Int)) !! Any;
    }
    multi method roll(Int(Cool) $num) {
        return self.list.roll($num)
          unless nqp::istype($!min, Int) && nqp::istype($!max, Numeric);

        my Int $least =
          $!excludes-min ?? $!min + 1 !! $!min;
        my Int $elems =
          1 + ($!excludes-max ?? $!max.Int - 1 !! $!max.Int) - $least;

        my int $todo = nqp::unbox_i($num.Int);
        if $elems {
            gather while $todo {
                take $least + nqp::rand_I(nqp::decont($elems), Int);
                $todo = $todo - 1;
            }
        }
        else {
            Any xx $todo;
        }
    }

    proto method pick(|)        { * }
    multi method pick()          { self.roll };
    multi method pick(Whatever)  { self.list.pick(*) };
    multi method pick(Int(Cool) $n) {
        return self.list.pick($n)
          unless nqp::istype($!min, Int) && nqp::istype($!max, Numeric);

        my Int $least =
          $!excludes-min ?? $!min + 1 !! $!min;
        my Int $elems =
          1 + ($!excludes-max ?? $!max.Int - 1 !! $!max.Int) - $least;
        my int $todo = nqp::unbox_i($n.Int);

        # faster to make list and then take from there
        return self.list.pick($n) if $elems < 3 * $todo;

        my %seen;
        gather while $todo {
            my Int $x  := $least + nqp::rand_I(nqp::decont($elems), Int);
            unless %seen.EXISTS-KEY($x) {
                %seen{$x} = 1;
                take $x;
                $todo = $todo - 1;
            }
        }
    }

    multi method Numeric(Range:D:) {
        return self.flat.elems unless nqp::istype($.max,Numeric) && nqp::istype($.min,Numeric);

        my $diff := $.max - $.min - $.excludes-min;

        # empty range
        return 0 if $diff < 0;

        my $floor := $diff.floor;
        $floor + 1 - ($floor == $diff ?? $.excludes-max !! 0);
    }

    method clone-with-op(&op, $value) {
        self.clone( :min($!min [&op] $value), :max($!max [&op] $value) );
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
