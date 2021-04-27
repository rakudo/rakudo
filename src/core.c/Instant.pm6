my class Date { ... }
my class DateTime { ... }
my class Duration {... }

my class Instant is Cool does Real {
    has Real $.tai is default(0);
      # A linear count of nanoseconds since 1970-01-01T00:00:00Z, plus
      # Rakudo::Internals.initial-offset. Thus, $.tai matches TAI from 1970
      # to the present.

    method new(*@) { X::Cannot::New.new(class => self).throw }

    method tai(--> Rat:D) {
        $!tai / 1000000000
    }

    method from-posix-nanos(Instant:U: Int:D $nanos --> Instant:D) {
        nqp::p6bindattrinvres(nqp::create(Instant),Instant,'$!tai',$nanos)
    }

    method to-nanos(--> Int:D) {
        $!tai.Int
    }

    proto method from-posix(|) {*}
    multi method from-posix($posix --> Instant:D) {
        nqp::p6bindattrinvres(nqp::create(Instant),Instant,'$!tai',
          (Rakudo::Internals.tai-from-posix($posix,0) * 1000000000).Int)
    }
    multi method from-posix($posix, Bool $prefer-leap-second --> Instant:D) {
    # $posix is in general not expected to be an integer.
    # If $prefer-leap-second is true, 915148800 is interpreted to
    # mean 1998-12-31T23:59:60Z rather than 1999-01-01T00:00:00Z.
        nqp::p6bindattrinvres(nqp::create(Instant),Instant,'$!tai',
          (Rakudo::Internals.tai-from-posix($posix,$prefer-leap-second) * 1000000000).Int)
    }

    method to-posix(--> List:D) {
    # The inverse of .from-posix, except that the second return
    # value is true if *and only if* this Instant is in a leap
    # second.
        Rakudo::Internals.posix-and-leap-from-tai($!tai / 1000000000)
    }

    multi method Str(Instant:D: --> Str:D) {
        'Instant:' ~ self.tai
    }
    multi method raku(Instant:D: --> Str:D) {
        my ($posix,$flag) = self.to-posix;
        'Instant.from-posix(' ~ $posix.raku ~ ($flag ?? ',True)' !! ')')
    }
    method Bridge(Instant:   --> Num:D) { self.defined ?? self.tai.Bridge !! self.Real::Bridge }
    method Num   (Instant:D: --> Num:D) { self.tai.Num    }
    method Rat   (Instant:D: --> Rat:D) { self.tai        }
    method Int   (Instant:D: --> Int:D) { self.tai.Int    }
    method narrow(Instant:D:          ) { self.tai.narrow }

    method Date(Instant:D:     --> Date:D)     { Date.new(self)     }
    method DateTime(Instant:D: --> DateTime:D) { DateTime.new(self) }
    method Instant() { self }

#    TODO: should be the new .gist, probably
#    method Str() {
#        'Instant:' ~ default-formatter
#            ::DateTime.new(self), :subseconds
#    }
}

multi sub infix:«cmp»(Instant:D $a, Instant:D $b) {
    $a.to-nanos <=> $b.to-nanos }

multi sub infix:«<=>»(Instant:D $a, Instant:D $b) {
    $a.to-nanos <=> $b.to-nanos
}

multi sub infix:«==»(Instant:D $a, Instant:D $b --> Bool:D) {
    $a.to-nanos == $b.to-nanos
}

multi sub infix:«!=»(Instant:D $a, Instant:D $b --> Bool:D) {
    $a.to-nanos != $b.to-nanos
}

multi sub infix:«<»(Instant:D $a, Instant:D $b --> Bool:D) {
    $a.to-nanos < $b.to-nanos
}

multi sub infix:«>»(Instant:D $a, Instant:D $b --> Bool:D) {
    $a.to-nanos > $b.to-nanos
}

multi sub infix:«<=»(Instant:D $a, Instant:D $b --> Bool:D) {
    $a.to-nanos <= $b.to-nanos
}

multi sub infix:«>=»(Instant:D $a, Instant:D $b --> Bool:D) {
    $a.to-nanos >= $b.to-nanos
}

multi sub infix:<+>(Instant:D $a, Instant:D $b) {
    die "Adding two Instant values has no meaning.
Did you mean to subtract?  Perhaps you need to convert to .Numeric first?"
}
multi sub infix:<+>(Instant:D $a, Real:D $b --> Instant:D) {
    Instant.from-posix-nanos($a.to-nanos + ($b * 1000000000).Int)
}
multi sub infix:<+>(Real:D $a, Instant:D $b --> Instant:D) {
    Instant.from-posix-nanos(($a * 1000000000).Int + $b.to-nanos)
}
multi sub infix:<+>(Instant:D $a, Duration:D $b --> Instant:D) {
    Instant.from-posix-nanos($a.to-nanos + $b.to-nanos)
}
multi sub infix:<+>(Duration:D $a, Instant:D $b --> Instant:D) {
    Instant.from-posix-nanos($a.to-nanos + $b.to-nanos)
}

multi sub infix:<->(Instant:D $a, Instant:D $b --> Duration:D) {
    Duration.from-posix-nanos($a.to-nanos - $b.to-nanos);
}
multi sub infix:<->(Instant:D $a, Real:D $b --> Instant:D) {
    Instant.from-posix-nanos($a.to-nanos - ($b * 1000000000).Int)
}

sub term:<time>(--> Int:D) { nqp::time() div 1000000000 }
# 37 is $initial-offset from Rakudo::Internals + # of years
# that have had leap seconds so far. Will need to be incremented
# when new leap seconds occur.
my int constant \tai-offset-nanos = 37 * 1000000000;
sub term:<now>(--> Instant:D) {
    # FIXME: During a leap second, the returned value is one
    # second greater than it should be.
    Instant.from-posix-nanos(nqp::add_i(nqp::time,tai-offset-nanos))
}

Rakudo::Internals.REGISTER-DYNAMIC: '$*INIT-INSTANT', {
    PROCESS::<$INIT-INSTANT> :=
        nqp::p6bindattrinvres(nqp::create(Instant),Instant,'$!tai',
          (Rakudo::Internals.tai-from-posix(Rakudo::Internals.INITTIME,0) * 1000000000).Int)
}

# vim: expandtab shiftwidth=4
