my class Date { ... }
my class DateTime { ... }
my class Duration {... }

my class Instant is Cool does Real {
    has Rat $.tai;
      # A linear count of seconds since 1970-01-01T00:00:00Z, plus
      # Rakudo::Internals.initial-offset. Thus, $.tai matches TAI from 1970
      # to the present.

    method new(*@) { X::Cannot::New.new(class => self).throw }

    proto method from-posix(|) {*}
    multi method from-posix($posix --> Instant:D) {
        nqp::p6bindattrinvres(nqp::create(Instant),Instant,'$!tai',
          Rakudo::Internals.tai-from-posix($posix,0).Rat)
    }
    multi method from-posix($posix, Bool $prefer-leap-second --> Instant:D) {
    # $posix is in general not expected to be an integer.
    # If $prefer-leap-second is true, 915148800 is interpreted to
    # mean 1998-12-31T23:59:60Z rather than 1999-01-01T00:00:00Z.
        nqp::p6bindattrinvres(nqp::create(Instant),Instant,'$!tai',
          Rakudo::Internals.tai-from-posix($posix,$prefer-leap-second).Rat)
    }

    method to-posix(--> List:D) {
    # The inverse of .from-posix, except that the second return
    # value is true if *and only if* this Instant is in a leap
    # second.
        Rakudo::Internals.posix-and-leap-from-tai($!tai)
    }

    multi method Str(Instant:D: --> Str:D) {
        'Instant:' ~ $!tai
    }
    multi method raku(Instant:D: --> Str:D) {
        my ($posix,$flag) = self.to-posix;
        'Instant.from-posix(' ~ $posix.raku ~ ($flag ?? ',True)' !! ')')
    }
    method Bridge(Instant:D:          ) { $!tai.Bridge }
    method Num   (Instant:D: --> Num:D) { $!tai.Num    }
    method Rat   (Instant:D: --> Rat:D) { $!tai        }
    method Int   (Instant:D: --> Int:D) { $!tai.Int    }
    method narrow(Instant:D:          ) { $!tai.narrow }

    method Date(Instant:D:     --> Date:D)     { Date.new(self)     }
    method DateTime(Instant:D: --> DateTime:D) { DateTime.new(self) }
    method Instant() { self }

#    TODO: should be the new .gist, probably
#    method Str() {
#        'Instant:' ~ default-formatter
#            ::DateTime.new(self), :subseconds
#    }
}

multi sub infix:«cmp»(Instant:D $a, Instant:D $b --> Order:D) {
    $a.tai <=> $b.tai }

multi sub infix:«<=>»(Instant:D $a, Instant:D $b --> Order:D) {
    $a.tai <=> $b.tai
}

multi sub infix:«==»(Instant:D $a, Instant:D $b --> Bool:D) {
    $a.tai == $b.tai
}

multi sub infix:«!=»(Instant:D $a, Instant:D $b --> Bool:D) {
    $a.tai != $b.tai
}

multi sub infix:«<»(Instant:D $a, Instant:D $b --> Bool:D) {
    $a.tai < $b.tai
}

multi sub infix:«>»(Instant:D $a, Instant:D $b --> Bool:D) {
    $a.tai > $b.tai
}

multi sub infix:«<=»(Instant:D $a, Instant:D $b --> Bool:D) {
    $a.tai <= $b.tai
}

multi sub infix:«>=»(Instant:D $a, Instant:D $b --> Bool:D) {
    $a.tai >= $b.tai
}

multi sub infix:<+>(Instant:D $a, Instant:D $b) {
    die "Adding two Instant values has no meaning.
Did you mean to subtract?  Perhaps you need to convert to .Numeric first?"
}
multi sub infix:<+>(Instant:D $a, Real:D $b --> Instant:D) {
    nqp::p6bindattrinvres(nqp::create(Instant),Instant,'$!tai',
      $a.tai + $b.Rat)
}
multi sub infix:<+>(Real:D $a, Instant:D $b --> Instant:D) {
    nqp::p6bindattrinvres(nqp::create(Instant),Instant,'$!tai',
      $a.Rat + $b.tai)
}
multi sub infix:<+>(Instant:D $a, Duration:D $b --> Instant:D) {
    nqp::p6bindattrinvres(nqp::create(Instant),Instant,'$!tai',
      $a.tai + $b.tai)
}
multi sub infix:<+>(Duration:D $a, Instant:D $b --> Instant:D) {
    nqp::p6bindattrinvres(nqp::create(Instant),Instant,'$!tai',
      $a.tai + $b.tai)
}

multi sub infix:<->(Instant:D $a, Instant:D $b --> Duration:D) {
    Duration.new: $a.tai - $b.tai;
}
multi sub infix:<->(Instant:D $a, Real:D $b --> Instant:D) {
    nqp::p6bindattrinvres(nqp::create(Instant),Instant,'$!tai',
      $a.tai - $b.Rat)
}

sub term:<time>(--> Int:D) { nqp::p6box_i(nqp::time_i()) }
sub term:<now>(--> Instant:D) {
    # FIXME: During a leap second, the returned value is one
    # second greater than it should be.
    nqp::p6bindattrinvres(nqp::create(Instant),Instant,'$!tai',
      Rakudo::Internals.tai-from-posix(nqp::time_n,0).Rat)
}

Rakudo::Internals.REGISTER-DYNAMIC: '$*INIT-INSTANT', {
    PROCESS::<$INIT-INSTANT> :=
        nqp::p6bindattrinvres(nqp::create(Instant),Instant,'$!tai',
          Rakudo::Internals.tai-from-posix(Rakudo::Internals.INITTIME,0).Rat)
}

# vim: expandtab shiftwidth=4
