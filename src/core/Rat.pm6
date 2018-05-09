# XXX: should be Rational[Int, uint]
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
    multi method Str(Rat:D:) {
        my $whole  = self.abs.floor;
        my $fract  = self.abs - $whole;

        # fight floating point noise issues RT#126016
        if $fract.Num == 1e0 { ++$whole; $fract = 0 }

        my $result = nqp::if(
            nqp::islt_I($!numerator, 0), '-', ''
        ) ~ $whole;

        if $fract {
            my $precision = $!denominator < 100_000
                ?? 5 !! $!denominator.Str.chars;

            my $fract-result = '';
            while $fract and $fract-result.chars < $precision {
                $fract *= 100;
                my $f   = $fract.floor;
                $fract -= $f;
                $fract-result ~= $f < 10 ?? "0$f" !!
                                 (!$fract and $f %% 10) ?? ($f / 10).floor !! $f;
            }
            if $fract and $fract-result.chars < $precision + 1 {
                $fract *= 10;
                given $fract.floor {
                    $fract-result ~= $_;
                    $fract        -= $_;
                }
            }
            ++$fract-result if 2*$fract >= 1; # round off fractional result

            $result ~= '.' ~ $fract-result;
        }
        $result
    }
}

my constant UINT64_UPPER = nqp::pow_I(2, 64, Num, Int);

my class FatRat is Cool does Rational[Int, Int] {
    method FatRat(FatRat:D: Real $?) { self }
    method Rat   (FatRat:D: Real $?) {
        $!denominator < UINT64_UPPER
          ?? Rat.new($!numerator, $!denominator)
          !! Failure.new("Cannot convert from FatRat to Rat because denominator is too big")
    }
    multi method perl(FatRat:D:) {
        "FatRat.new($!numerator, $!denominator)";
    }
    multi method Str(FatRat:D:) {
        my $whole  = self.abs.floor;
        my $fract  = self.abs - $whole;

        my $result = nqp::if(
            nqp::islt_I($!numerator, 0), '-', ''
        ) ~ $whole;

        if $fract {
            if $!denominator.chars < 5 {
                return self.Rat.Str;
            } else {
                my $c = $!denominator.chars + 1;
                $fract *= 10 ** $c;
                my $f   = ($fract + .5).floor;
                my $fc = $f.chars;
                $f.=subst(/ ^ 1 0+ $ /, 1); # Reduce powers of 10
                $result ~= '.' ~ '0' x ($c - $fc) ~ $f;
            }
        }
        $result;
    }
}

sub DIVIDE_NUMBERS(Int:D \nu, Int:D \de, \t1, \t2) {
    nqp::stmts(
      (my Int $gcd         := de == 0 ?? 1 !! nu gcd de),
      (my Int $numerator   := nu div $gcd),
      (my Int $denominator := de div $gcd),
      nqp::if(
        $denominator < 0,
        nqp::stmts(
          ($numerator   := -$numerator),
          ($denominator := -$denominator))),
      nqp::if(
        nqp::istype(t1, FatRat) || nqp::istype(t2, FatRat),
        nqp::p6bindattrinvres(
          nqp::p6bindattrinvres(nqp::create(FatRat),FatRat,'$!numerator',$numerator),
          FatRat,'$!denominator',$denominator),
        nqp::if(
          $denominator < UINT64_UPPER,
          nqp::p6bindattrinvres(
            nqp::p6bindattrinvres(nqp::create(Rat),Rat,'$!numerator',$numerator),
            Rat,'$!denominator',$denominator),
          nqp::p6box_n(nqp::div_In($numerator, $denominator)))))
}

sub DON'T_DIVIDE_NUMBERS(Int:D \nu, Int:D \de, \t1, \t2) {
    nqp::istype(t1, FatRat) || nqp::istype(t2, FatRat)
        ?? nqp::p6bindattrinvres(
              nqp::p6bindattrinvres(
                  nqp::create(FatRat),
                  FatRat, '$!numerator', nqp::decont(nu)),
              FatRat, '$!denominator', nqp::decont(de))
        !! nqp::p6bindattrinvres(
              nqp::p6bindattrinvres(
                  nqp::create(Rat),
                  Rat, '$!numerator', nqp::decont(nu)),
              Rat, '$!denominator', nqp::decont(de))
}

multi sub prefix:<->(Rat:D \a) {
    Rat.new(-a.numerator, a.denominator);
}
multi sub prefix:<->(FatRat:D \a) {
    FatRat.new(-a.numerator, a.denominator);
}

multi sub infix:<+>(Rational:D \a, Rational:D \b) {
    a.denominator == b.denominator
        ?? DON'T_DIVIDE_NUMBERS(a.numerator + b.numerator, a.denominator, a, b)
        !! DIVIDE_NUMBERS
            a.numerator*b.denominator + b.numerator*a.denominator,
            a.denominator*b.denominator,
            a,
            b
}
multi sub infix:<+>(Rational:D \a, Int:D \b) {
    DON'T_DIVIDE_NUMBERS(
        (a.numerator + b * a.denominator),
        a.denominator,
        a,
        b,
    );
}
multi sub infix:<+>(Int:D \a, Rational:D \b) {
    DON'T_DIVIDE_NUMBERS(
        (a * b.denominator + b.numerator),
        b.denominator,
        a,
        b,
    );
}

multi sub infix:<->(Rational:D \a, Rational:D \b) {
    a.denominator == b.denominator
        ?? DON'T_DIVIDE_NUMBERS(a.numerator - b.numerator, a.denominator, a, b)
        !! DIVIDE_NUMBERS
            a.numerator*b.denominator - b.numerator*a.denominator,
            a.denominator*b.denominator,
            a,
            b
}

multi sub infix:<->(Rational:D \a, Int:D \b) {
    DON'T_DIVIDE_NUMBERS
        a.numerator - b * a.denominator,
        a.denominator,
        a,
        b;
}

multi sub infix:<->(Int:D \a, Rational:D \b) {
    DON'T_DIVIDE_NUMBERS
        a * b.denominator - b.numerator,
        b.denominator,
        a,
        b;
}

multi sub infix:<*>(Rational:D \a, Rational:D \b) {
    DIVIDE_NUMBERS
        a.numerator * b.numerator,
        a.denominator * b.denominator,
        a,
        b;
}

multi sub infix:<*>(Rational:D \a, Int:D \b) {
    DIVIDE_NUMBERS
        a.numerator * b,
        a.denominator,
        a,
        b;
}

multi sub infix:<*>(Int:D \a, Rational:D \b) {
    DIVIDE_NUMBERS
        a * b.numerator,
        b.denominator,
        a,
        b;
}

multi sub infix:</>(Rational:D \a, Rational:D \b) {
    DIVIDE_NUMBERS
        a.numerator * b.denominator,
        a.denominator * b.numerator,
        a,
        b;
}

multi sub infix:</>(Rational:D \a, Int:D \b) {
    DIVIDE_NUMBERS
        a.numerator,
        a.denominator * b,
        a,
        b;
}

multi sub infix:</>(Int:D \a, Rational:D \b) {
    b.REDUCE-ME; # RT #126391: [BUG] Bad "divide by 0" error message
    DIVIDE_NUMBERS
        b.denominator * a,
        b.numerator,
        a,
        b;
}

multi sub infix:</>(Int:D \a, Int:D \b) {
    DIVIDE_NUMBERS a, b, a, b
}

multi sub infix:<%>(Rational:D \a, Int:D \b) {
    a - floor(a / b) * b
}

multi sub infix:<%>(Int:D \a, Rational:D \b) {
    a - floor(a / b) * b
}

multi sub infix:<%>(Rational:D \a, Rational:D \b) {
    a - floor(a / b) * b
}

multi sub infix:<**>(Rational:D \a, Int:D \b) {
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
