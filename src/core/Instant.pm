my class Date { ... }
my class DateTime { ... }
my class Duration {... }

my class Instant is Cool does Real {
    has Rat $.tai;
      # A linear count of seconds since 1970-01-01T00:00:00Z, plus
      # Rakudo::Internals.initial-offset. Thus, $.tai matches TAI from 1970
      # to the present.

    method SET-SELF($!tai) { self } # cannot be private because of operators

    method new(*@) { X::Cannot::New.new(class => self).throw }

    method from-posix($posix, Bool $prefer-leap-second = False) {
    # $posix is in general not expected to be an integer.
    # If $prefer-leap-second is true, 915148800 is interpreted to
    # mean 1998-12-31T23:59:60Z rather than 1999-01-01T00:00:00Z.
        nqp::create(Instant).SET-SELF(
          Rakudo::Internals.tai-from-posix($posix,$prefer-leap-second).Rat
        )
    }

    method to-posix() {
    # The inverse of .from-posix, except that the second return
    # value is true if *and only if* this Instant is in a leap
    # second.
        Rakudo::Internals.posix-from-tai($!tai)
    }

    multi method Str(Instant:D:) {
        'Instant:' ~ $!tai
    }
    multi method perl(Instant:D:) {
        "Instant.from-posix{self.to-posix.perl}";
    }
    method Bridge(Instant:D:) { $!tai.Bridge }
    method Num   (Instant:D:) { $!tai.Num    }
    method Int   (Instant:D:) { $!tai.Int    }
    method narrow(Instant:D:) { $!tai.narrow }

    method Date(Instant:D:)        { Date.new(self)         }
    method DateTime(Instant:D: |c) { DateTime.new(self, |c) }

#    TODO: should be the new .gist, probably
#    method Str() {
#        'Instant:' ~ default-formatter
#            ::DateTime.new(self), :subseconds
#    }
}

multi sub infix:«cmp»(Instant:D $a, Instant:D $b) {
    $a.tai <=> $b.tai }

multi sub infix:«<=>»(Instant:D $a, Instant:D $b) {
    $a.tai <=> $b.tai
}

multi sub infix:«==»(Instant:D $a, Instant:D $b) {
    $a.tai == $b.tai
}

multi sub infix:«!=»(Instant:D $a, Instant:D $b) {
    $a.tai != $b.tai
}

multi sub infix:«<»(Instant:D $a, Instant:D $b) {
    $a.tai < $b.tai
}

multi sub infix:«>»(Instant:D $a, Instant:D $b) {
    $a.tai > $b.tai
}

multi sub infix:«<=»(Instant:D $a, Instant:D $b) {
    $a.tai <= $b.tai
}

multi sub infix:«>=»(Instant:D $a, Instant:D $b) {
    $a.tai >= $b.tai
}

multi sub infix:<+>(Instant:D $a, Real:D $b) {
    nqp::create(Instant).SET-SELF($a.tai + $b.Rat)
}
multi sub infix:<+>(Real:D $a, Instant:D $b) {
    nqp::create(Instant).SET-SELF($a.Rat + $b.tai)
}
multi sub infix:<+>(Instant:D $a, Duration:D $b) {
    nqp::create(Instant).SET-SELF($a.tai + $b.tai)
}
multi sub infix:<+>(Duration:D $a, Instant:D $b) {
    nqp::create(Instant).SET-SELF($a.tai + $b.tai)
}

multi sub infix:<->(Instant:D $a, Instant:D $b) {
    Duration.new: $a.tai - $b.tai;
}
multi sub infix:<->(Instant:D $a, Real:D $b) {
    nqp::create(Instant).SET-SELF($a.tai - $b.Rat)
}

sub term:<time>() { nqp::p6box_i(nqp::time_i()) }
sub term:<now>() {
    # FIXME: During a leap second, the returned value is one
    # second greater than it should be.
    Instant.from-posix: nqp::time_n
}

Rakudo::Internals.REGISTER-DYNAMIC: '$*INITTIME', {
    PROCESS::<$INITTIME> := Instant.from-posix: Rakudo::Internals.INITTIME;
}

# vim: ft=perl6 expandtab sw=4
