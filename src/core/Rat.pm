# XXX: should be Rational[Int, UInt64]
my class Rat is Cool does Rational[Int, Int] {
    method Rat   (Rat:D: Real $?) { self }
    method FatRat(Rat:D: Real $?) { FatRat.new($!numerator, $!denominator); }
    multi method perl(Rat:D:) {
        if $!denominator == 1 {
            $!numerator ~ '.0'
        }
        else {
            my $d = $!denominator;
            unless $d == 0 {
                $d = $d div 5 while $d %% 5;
                $d = $d div 2 while $d %% 2;
                self.REDUCE-ME;
            }
            if $d == 1 and (my $b := self.base(10,*)).Numeric === self {
                $b;
            }
            else {
                '<' ~ $!numerator ~ '/' ~ $!denominator ~ '>'
            }
        }
    }
}

my class FatRat is Cool does Rational[Int, Int] {
    method FatRat(FatRat:D: Real $?) { self }
    method Rat   (FatRat:D: Real $?) {
        $!denominator < $UINT64_UPPER
          ?? Rat.new($!numerator, $!denominator)
          !! Failure.new("Cannot convert from FatRat to Rat because denominator is too big")
    }
    multi method perl(FatRat:D:) {
        "FatRat.new($!numerator, $!denominator)";
    }
}

sub DIVIDE_NUMBERS(Int:D \nu, Int:D \de, $t1, $t2) {
    my Int $gcd         := de == 0 ?? 1 !! nu gcd de;
    my Int $numerator   := nu div $gcd;
    my Int $denominator := de div $gcd;
    my $r;
    if $denominator < 0 {
        $numerator   := -$numerator;
        $denominator := -$denominator;
    }
    if nqp::istype($t1, FatRat) || nqp::istype($t2, FatRat) {
        $r := nqp::create(FatRat);
        nqp::bindattr($r, FatRat, '$!numerator',   nqp::decont($numerator));
        nqp::bindattr($r, FatRat, '$!denominator', nqp::decont($denominator));
    } elsif $denominator < $UINT64_UPPER {
        $r := nqp::create(Rat);
        nqp::bindattr($r, Rat, '$!numerator',   nqp::decont($numerator));
        nqp::bindattr($r, Rat, '$!denominator', nqp::decont($denominator));
    } else {
        $r := nqp::p6box_n(nqp::div_In(
                nqp::decont($numerator),
                nqp::decont($denominator)
            )
        );
    }
    $r;
}

sub DON'T_DIVIDE_NUMBERS(Int:D \nu, Int:D \de, $t1, $t2) {
    my $r;
    if nqp::istype($t1, FatRat) || nqp::istype($t2, FatRat) {
        $r := nqp::create(FatRat);
        nqp::bindattr($r, FatRat, '$!numerator',   nqp::decont(nu));
        nqp::bindattr($r, FatRat, '$!denominator', nqp::decont(de));
    } else {
        $r := nqp::create(Rat);
        nqp::bindattr($r, Rat, '$!numerator',   nqp::decont(nu));
        nqp::bindattr($r, Rat, '$!denominator', nqp::decont(de));
    }
    $r;
}

multi sub prefix:<->(Rat:D \a) {
    Rat.new(-a.numerator, a.denominator);
}
multi sub prefix:<->(FatRat:D \a) {
    FatRat.new(-a.numerator, a.denominator);
}

multi sub infix:<+>(Rational \a, Rational \b) {
    if a.denominator == b.denominator {
        DON'T_DIVIDE_NUMBERS(a.numerator + b.numerator, a.denominator, a, b);
    }
    else {
        my Int $gcd := a.denominator gcd b.denominator;
        DIVIDE_NUMBERS(
            (a.numerator * (b.denominator div $gcd) + b.numerator * (a.denominator div $gcd)),
            ((a.denominator div $gcd) * b.denominator),
            a,
            b,
        );
    }
}
multi sub infix:<+>(Rational \a, Int \b) {
    DON'T_DIVIDE_NUMBERS(
        (a.numerator + b * a.denominator),
        a.denominator,
        a,
        b,
    );
}
multi sub infix:<+>(Int \a, Rational \b) {
    DON'T_DIVIDE_NUMBERS(
        (a * b.denominator + b.numerator),
        b.denominator,
        a,
        b,
    );
}

multi sub infix:<->(Rational \a, Rational \b) {
    if a.denominator == b.denominator {
        DON'T_DIVIDE_NUMBERS(a.numerator - b.numerator, a.denominator, a, b);
    }
    else {
        my Int $gcd = a.denominator gcd b.denominator;
        DIVIDE_NUMBERS
            a.numerator * (b.denominator div $gcd) - b.numerator * (a.denominator div $gcd),
            (a.denominator div $gcd) * b.denominator,
            a,
            b;
    }
}

multi sub infix:<->(Rational \a, Int \b) {
    DON'T_DIVIDE_NUMBERS
        a.numerator - b * a.denominator,
        a.denominator,
        a,
        b;
}

multi sub infix:<->(Int \a, Rational \b) {
    DON'T_DIVIDE_NUMBERS
        a * b.denominator - b.numerator,
        b.denominator,
        a,
        b;
}

multi sub infix:<*>(Rational \a, Rational \b) {
    DIVIDE_NUMBERS
        a.numerator * b.numerator,
        a.denominator * b.denominator,
        a,
        b;
}

multi sub infix:<*>(Rational \a, Int \b) {
    DIVIDE_NUMBERS
        a.numerator * b,
        a.denominator,
        a,
        b;
}

multi sub infix:<*>(Int \a, Rational \b) {
    DIVIDE_NUMBERS
        a * b.numerator,
        b.denominator,
        a,
        b;
}

multi sub infix:</>(Rational \a, Rational \b) {
    DIVIDE_NUMBERS
        a.numerator * b.denominator,
        a.denominator * b.numerator,
        a,
        b;
}

multi sub infix:</>(Rational \a, Int \b) {
    DIVIDE_NUMBERS
        a.numerator,
        a.denominator * b,
        a,
        b;
}

multi sub infix:</>(Int \a, Rational \b) {
    b.REDUCE-ME; # RT #126391: [BUG] Bad "divide by 0" error message
    DIVIDE_NUMBERS
        b.denominator * a,
        b.numerator,
        a,
        b;
}

multi sub infix:</>(Int \a, Int \b) {
    DIVIDE_NUMBERS a, b, a, b
}

multi sub infix:<%>(Rational \a, Int \b) {
    a - floor(a / b) * b
}

multi sub infix:<%>(Int \a, Rational \b) {
    a - floor(a / b) * b
}

multi sub infix:<%>(Rational \a, Rational \b) {
    a - floor(a / b) * b
}

multi sub infix:<**>(Rational \a, Int \b) {
    b >= 0
        ?? DIVIDE_NUMBERS
            (a.numerator ** b // fail (a.numerator.abs > a.denominator ?? X::Numeric::Overflow !! X::Numeric::Underflow).new),
            a.denominator ** b,  # we presume it likely already blew up on the numerator
            a,
            b
        !! DIVIDE_NUMBERS
            (a.denominator ** -b // fail (a.numerator.abs < a.denominator ?? X::Numeric::Overflow !! X::Numeric::Underflow).new),
            a.numerator ** -b,
            a,
            b
}

multi sub infix:<==>(Rational:D \a, Rational:D \b) {
    nqp::isfalse(a.denominator) || nqp::isfalse(b.denominator)
        ?? a.Num == b.Num
        !! a.numerator * b.denominator == b.numerator * a.denominator
}
multi sub infix:<==>(Rational:D \a, Int:D \b) {
    a.REDUCE-ME;
    a.numerator == b && a.denominator == 1
}
multi sub infix:<==>(Int:D \a, Rational:D \b) {
    b.REDUCE-ME;
    a == b.numerator && b.denominator == 1;
}
multi sub infix:<===>(Rational:D \a, Rational:D \b --> Bool:D) {
    # Check whether we have 0-denominator rationals as well. Those can
    # be `==` but have different numerator values and so should not `===` True.
    # Since we're already checking equality first, we only need to check the
    # zeroeness of the denominator of just one parameter
    a.WHAT =:= b.WHAT
        && (a == b || (a.isNaN && b.isNaN))
        && (a.denominator.Bool || a.numerator == b.numerator)
}

multi sub infix:«<»(Rational:D \a, Rational:D \b) {
    a.numerator * b.denominator < b.numerator * a.denominator
}
multi sub infix:«<»(Rational:D \a, Int:D \b) {
    a.numerator  < b * a.denominator
}
multi sub infix:«<»(Int:D \a, Rational:D \b) {
    a * b.denominator < b.numerator
}

multi sub infix:«<=»(Rational:D \a, Rational:D \b) {
    a.numerator * b.denominator <= b.numerator * a.denominator
}
multi sub infix:«<=»(Rational:D \a, Int:D \b) {
    a.numerator  <= b * a.denominator
}
multi sub infix:«<=»(Int:D \a, Rational:D \b) {
    a * b.denominator <= b.numerator
}

multi sub infix:«>»(Rational:D \a, Rational:D \b) {
    a.numerator * b.denominator > b.numerator * a.denominator
}
multi sub infix:«>»(Rational:D \a, Int:D \b) {
    a.numerator  > b * a.denominator
}
multi sub infix:«>»(Int:D \a, Rational:D \b) {
    a * b.denominator > b.numerator
}

multi sub infix:«>=»(Rational:D \a, Rational:D \b) {
    a.numerator * b.denominator >= b.numerator * a.denominator
}
multi sub infix:«>=»(Rational:D \a, Int:D \b) {
    a.numerator  >= b * a.denominator
}
multi sub infix:«>=»(Int:D \a, Rational:D \b) {
    a * b.denominator >= b.numerator
}

multi sub infix:«<=>»(Rational:D \a, Rational:D \b) {
    a.numerator * b.denominator <=> b.numerator * a.denominator
}
multi sub infix:«<=>»(Rational:D \a, Int:D \b) {
    a.numerator  <=> b * a.denominator
}
multi sub infix:«<=>»(Int:D \a, Rational:D \b) {
    a * b.denominator <=> b.numerator
}

# vim: ft=perl6 expandtab sw=4
