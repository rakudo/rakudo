# XXX: should be Rational[Int, uint]
my class Rat is Cool does Rational[Int, Int] {
    method Rat(Rat:D: Real $? --> Rat:D) {
        self
    }
    method FatRat(Rat:D: Real $? --> FatRat:D) {
        FatRat.new($!numerator, $!denominator)
    }
    multi method perl(Rat:D: --> Str:D) {
        if $!denominator == 1 {
            $!numerator ~ '.0'
        }
        else {
            my $d = $!denominator;
            unless $d == 0 {
                $d = $d div 5 while $d %% 5;
                $d = $d div 2 while $d %% 2;
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

my constant UINT64_UPPER = nqp::pow_I(2, 64, Num, Int);

my class FatRat is Cool does Rational[Int, Int] {
    method FatRat(FatRat:D: Real $? --> FatRat) {
        self
    }
    method Rat(FatRat:D: Real $? --> Rat:D) {
        $!denominator < UINT64_UPPER
          ?? Rat.new($!numerator, $!denominator)
          !! Failure.new("Cannot convert from FatRat to Rat because denominator is too big")
    }
    multi method perl(FatRat:D: --> Str:D) {
        "FatRat.new($!numerator, $!denominator)";
    }
}

sub DIVIDE_NUMBERS(Int:D \nu, Int:D \de, \t1, \t2) {
    nqp::stmts(
      (my $numerator),
      (my $denominator),
      nqp::if(
        de,
        nqp::stmts(
          (my $gcd := nqp::gcd_I(nqp::decont(nu), nqp::decont(de), Int)),
          ($numerator   := nqp::div_I(nqp::decont(nu), $gcd, Int)),
          ($denominator := nqp::div_I(nqp::decont(de), $gcd, Int)),
          nqp::if(
            nqp::islt_I($denominator, 0),
            nqp::stmts(
              ($numerator   := nqp::neg_I($numerator, Int)),
              ($denominator := nqp::neg_I($denominator, Int))))),
        nqp::stmts(
          ($numerator   := nqp::box_i(
            nqp::isgt_I(nqp::decont(nu), 0) ?? 1 !! nu ?? -1 !! 0, Int)),
          ($denominator := nqp::decont(de)))),
      RAKUDO_INTERNAL_DIVIDE_NUMBERS_NO_NORMALIZE
        $numerator, $denominator, t1, t2)
}

# ALL RATIONALS MUST BE NORMALIZED, however in some operations we cannot
# ever get a non-normalized Rational, if we start with a normalized Rational.
# For such cases, we can use this routine, to bypass normalization step,
# which would be useless.
sub RAKUDO_INTERNAL_DIVIDE_NUMBERS_NO_NORMALIZE(\nu, \de, \t1, \t2) {
    nqp::if(
      nqp::istype(t1, FatRat) || nqp::istype(t2, FatRat),
      nqp::p6bindattrinvres(
        nqp::p6bindattrinvres(nqp::create(FatRat),
          FatRat,'$!numerator',nu),
        FatRat,'$!denominator',de),
      nqp::if(
        nqp::islt_I(de, UINT64_UPPER),
        nqp::p6bindattrinvres(
          nqp::p6bindattrinvres(nqp::create(Rat),
            Rat,'$!numerator',nu),
          Rat,'$!denominator',de),
        nqp::p6box_n(nqp::div_In(nu, de))))
}

multi sub prefix:<->(Rat:D \a --> Rat:D) {
#    Rat.new(-a.numerator, a.denominator);
    nqp::p6bindattrinvres(
      nqp::clone(nqp::decont(a)),
      Rat,
      '$!numerator',
      nqp::neg_I(nqp::getattr(nqp::decont(a),Rat,'$!numerator'),Int)
    )
}
multi sub prefix:<->(FatRat:D \a --> FatRat:D) {
#    FatRat.new(-a.numerator, a.denominator);
    nqp::p6bindattrinvres(
      nqp::clone(nqp::decont(a)),
      FatRat,
      '$!numerator',
      nqp::neg_I(nqp::getattr(nqp::decont(a),FatRat,'$!numerator'),Int)
    )
}

multi sub infix:<+>(Rational:D \a, Rational:D \b) {
    my \adenom := nqp::getattr(nqp::decont(a),a.WHAT,'$!denominator');
    my \bdenom := nqp::getattr(nqp::decont(b),b.WHAT,'$!denominator');
    DIVIDE_NUMBERS(
      nqp::getattr(nqp::decont(a),a.WHAT,'$!numerator') * bdenom
        + nqp::getattr(nqp::decont(b),b.WHAT,'$!numerator') * adenom,
      adenom * bdenom, a, b
    )
}
multi sub infix:<+>(Rational:D \a, Int:D \b) {
    my \adenom := nqp::getattr(nqp::decont(a),a.WHAT,'$!denominator');
    DIVIDE_NUMBERS(
      nqp::getattr(nqp::decont(a),a.WHAT,'$!numerator') + b * adenom,
      adenom, a, b
    )
}
multi sub infix:<+>(Int:D \a, Rational:D \b) {
    my \bdenom := nqp::getattr(nqp::decont(b),b.WHAT,'$!denominator');
    DIVIDE_NUMBERS(
      a * bdenom + nqp::getattr(nqp::decont(b),b.WHAT,'$!numerator'),
      bdenom, a, b
    )
}

multi sub infix:<->(Rational:D \a, Rational:D \b) {
    my \adenom := nqp::getattr(nqp::decont(a),a.WHAT,'$!denominator');
    my \bdenom := nqp::getattr(nqp::decont(b),b.WHAT,'$!denominator');
    DIVIDE_NUMBERS(
      nqp::getattr(nqp::decont(a),a.WHAT,'$!numerator') * bdenom
        - nqp::getattr(nqp::decont(b),b.WHAT,'$!numerator') * adenom,
      adenom * bdenom, a, b
    )
}

multi sub infix:<->(Rational:D \a, Int:D \b) {
    my \adenom := nqp::getattr(nqp::decont(a),a.WHAT,'$!denominator');
    DIVIDE_NUMBERS(
      nqp::getattr(nqp::decont(a),a.WHAT,'$!numerator') - b * adenom,
      adenom, a, b
    )
}

multi sub infix:<->(Int:D \a, Rational:D \b) {
    my \bdenom := nqp::getattr(nqp::decont(b),b.WHAT,'$!denominator');
    DIVIDE_NUMBERS(
      a * bdenom - nqp::getattr(nqp::decont(b),b.WHAT,'$!numerator'),
      bdenom, a, b
    )
}

multi sub infix:<*>(Rational:D \a, Rational:D \b) {
    DIVIDE_NUMBERS
        a.numerator * b.numerator,
        a.denominator * b.denominator,
        a,
        b
}

multi sub infix:<*>(Rational:D \a, Int:D \b) {
    DIVIDE_NUMBERS
        a.numerator * b,
        a.denominator,
        a,
        b
}

multi sub infix:<*>(Int:D \a, Rational:D \b) {
    DIVIDE_NUMBERS
        a * b.numerator,
        b.denominator,
        a,
        b
}

multi sub infix:</>(Rational:D \a, Rational:D \b) {
    DIVIDE_NUMBERS
        a.numerator * b.denominator,
        a.denominator * b.numerator,
        a,
        b
}

multi sub infix:</>(Rational:D \a, Int:D \b) {
    DIVIDE_NUMBERS
        a.numerator,
        a.denominator * b,
        a,
        b
}

multi sub infix:</>(Int:D \a, Rational:D \b) {
    DIVIDE_NUMBERS
        b.denominator * a,
        b.numerator,
        a,
        b
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
    my $nu;
    my $de;
    nqp::if(
      nqp::isge_I(nqp::decont(b), 0),
        nqp::if( # if we got Inf
          nqp::istype(($nu := nqp::pow_I(a.numerator, nqp::decont(b), Num, Int)), Num),
          Failure.new(X::Numeric::Overflow.new),
          nqp::if( # if we got Inf
            nqp::istype(($de := nqp::pow_I(a.denominator, nqp::decont(b), Num, Int)), Num),
            Failure.new(X::Numeric::Overflow.new),
            RAKUDO_INTERNAL_DIVIDE_NUMBERS_NO_NORMALIZE $nu, $de, a, b)),
        nqp::if( # if we got Inf
          nqp::istype(($nu := nqp::pow_I(a.numerator,
            nqp::neg_I(nqp::decont(b), Int), Num, Int)), Num),
          Failure.new(X::Numeric::Underflow.new),
          nqp::if( # if we got Inf
            nqp::istype(($de := nqp::pow_I(a.denominator,
              nqp::neg_I(nqp::decont(b), Int), Num, Int)), Num),
            Failure.new(X::Numeric::Underflow.new),
            RAKUDO_INTERNAL_DIVIDE_NUMBERS_NO_NORMALIZE $de, $nu, a, b)))
}

multi sub infix:<==>(Rational:D \a, Rational:D \b --> Bool:D) {
    nqp::hllbool(
      nqp::iseq_I(
        (my \anum := nqp::getattr(nqp::decont(a),Rat,'$!numerator')),
        nqp::getattr(nqp::decont(b),Rat,'$!numerator')
      ) && nqp::iseq_I(
             (my \adenom := nqp::getattr(nqp::decont(a),Rat,'$!denominator')),
             nqp::getattr(nqp::decont(b),Rat,'$!denominator')
          ) && (                         # num/denom both same
                 nqp::istrue(anum)       # 1/X, Inf == Inf also true
                 || nqp::istrue(adenom)  # 0/1, NaN == NaN becomes false
               )
    )
}
multi sub infix:<==>(Rational:D \a, Int:D \b --> Bool:D) {
    nqp::hllbool(
      nqp::iseq_I(
        nqp::getattr(nqp::decont(a),Rat,'$!denominator'),
        1
      ) && nqp::iseq_I(
             nqp::getattr(nqp::decont(a),Rat,'$!numerator'),
             nqp::decont(b)
           )
    )
}
multi sub infix:<==>(Int:D \a, Rational:D \b --> Bool:D) {
    nqp::hllbool(
      nqp::iseq_I(
        nqp::getattr(nqp::decont(b),Rat,'$!denominator'),
        1
      ) && nqp::iseq_I(
             nqp::decont(a),
             nqp::getattr(nqp::decont(b),Rat,'$!numerator')
           )
    )
}
multi sub infix:<===>(Rational:D \a, Rational:D \b --> Bool:D) {
    nqp::hllbool(
      nqp::eqaddr(a.WHAT, b.WHAT)
        && nqp::iseq_I(
             nqp::getattr(nqp::decont(a),Rat,'$!numerator'),
             nqp::getattr(nqp::decont(b),Rat,'$!numerator')
        ) && nqp::iseq_I(
               nqp::getattr(nqp::decont(a),Rat,'$!denominator'),
               nqp::getattr(nqp::decont(b),Rat,'$!denominator')
            )
    )
}

multi sub infix:«<»(Rational:D \a, Rational:D \b --> Bool:D) {
#    a.numerator * b.denominator < b.numerator * a.denominator
    nqp::hllbool(       
      nqp::islt_I(
        nqp::mul_I(
          nqp::getattr(nqp::decont(a),Rat,'$!numerator'),
          nqp::getattr(nqp::decont(b),Rat,'$!denominator'),
          Int
        ),
        nqp::mul_I(
          nqp::getattr(nqp::decont(b),Rat,'$!numerator'),
          nqp::getattr(nqp::decont(a),Rat,'$!denominator'),
          Int
        )
      )
    )
}
multi sub infix:«<»(Rational:D \a, Int:D \b --> Bool:D) {
#    a.numerator  < b * a.denominator
    nqp::hllbool(       
      nqp::islt_I(
        nqp::getattr(nqp::decont(a),Rat,'$!numerator'),
        nqp::mul_I(
          nqp::decont(b),
          nqp::getattr(nqp::decont(a),Rat,'$!denominator'),
          Int
        )
      )
    )
}
multi sub infix:«<»(Int:D \a, Rational:D \b --> Bool:D) {
#    a * b.denominator < b.numerator
    nqp::hllbool(       
      nqp::islt_I(
        nqp::mul_I(
          nqp::decont(a),
          nqp::getattr(nqp::decont(b),Rat,'$!denominator'),
          Int
        ),
        nqp::getattr(nqp::decont(b),Rat,'$!numerator')
      )
    )
}

multi sub infix:«<=»(Rational:D \a, Rational:D \b --> Bool:D) {
#    a.numerator * b.denominator <= b.numerator * a.denominator
    nqp::hllbool(
      nqp::isle_I(
        nqp::mul_I(
          nqp::getattr(nqp::decont(a),Rat,'$!numerator'),
          nqp::getattr(nqp::decont(b),Rat,'$!denominator'),
          Int
        ),
        nqp::mul_I(
          nqp::getattr(nqp::decont(b),Rat,'$!numerator'),
          nqp::getattr(nqp::decont(a),Rat,'$!denominator'),
          Int
        )
      )
    )
}
multi sub infix:«<=»(Rational:D \a, Int:D \b --> Bool:D) {
#    a.numerator  <= b * a.denominator
    nqp::hllbool(       
      nqp::isle_I(
        nqp::getattr(nqp::decont(a),Rat,'$!numerator'),
        nqp::mul_I(
          nqp::decont(b),
          nqp::getattr(nqp::decont(a),Rat,'$!denominator'),
          Int
        )
      )
    )
}
multi sub infix:«<=»(Int:D \a, Rational:D \b --> Bool:D) {
#    a * b.denominator <= b.numerator
    nqp::hllbool(
      nqp::isle_I(
        nqp::mul_I(
          nqp::decont(a),
          nqp::getattr(nqp::decont(b),Rat,'$!denominator'),
          Int
        ),
        nqp::getattr(nqp::decont(b),Rat,'$!numerator')
      )
    )
}

multi sub infix:«>»(Rational:D \a, Rational:D \b --> Bool:D) {
#    a.numerator * b.denominator > b.numerator * a.denominator
    nqp::hllbool(
      nqp::isgt_I(
        nqp::mul_I(
          nqp::getattr(nqp::decont(a),Rat,'$!numerator'),
          nqp::getattr(nqp::decont(b),Rat,'$!denominator'),
          Int
        ),
        nqp::mul_I(
          nqp::getattr(nqp::decont(b),Rat,'$!numerator'),
          nqp::getattr(nqp::decont(a),Rat,'$!denominator'),
          Int
        )
      )
    )
}
multi sub infix:«>»(Rational:D \a, Int:D \b --> Bool:D) {
#    a.numerator  > b * a.denominator
    nqp::hllbool(
      nqp::isgt_I(
        nqp::getattr(nqp::decont(a),Rat,'$!numerator'),
        nqp::mul_I(
          nqp::decont(b),
          nqp::getattr(nqp::decont(a),Rat,'$!denominator'),
          Int
        )
      )
    )
}
multi sub infix:«>»(Int:D \a, Rational:D \b --> Bool:D) {
#    a * b.denominator > b.numerator
    nqp::hllbool(
      nqp::isgt_I(
        nqp::mul_I(
          nqp::decont(a),
          nqp::getattr(nqp::decont(b),Rat,'$!denominator'),
          Int
        ),
        nqp::getattr(nqp::decont(b),Rat,'$!numerator')
      )
    )
}

multi sub infix:«>=»(Rational:D \a, Rational:D \b --> Bool:D) {
#    a.numerator * b.denominator >= b.numerator * a.denominator
    nqp::hllbool(
      nqp::isge_I(
        nqp::mul_I(
          nqp::getattr(nqp::decont(a),Rat,'$!numerator'),
          nqp::getattr(nqp::decont(b),Rat,'$!denominator'),
          Int
        ),
        nqp::mul_I(
          nqp::getattr(nqp::decont(b),Rat,'$!numerator'),
          nqp::getattr(nqp::decont(a),Rat,'$!denominator'),
          Int
        )
      )
    )
}
multi sub infix:«>=»(Rational:D \a, Int:D \b --> Bool:D) {
#    a.numerator  >= b * a.denominator
    nqp::hllbool(
      nqp::isge_I(
        nqp::getattr(nqp::decont(a),Rat,'$!numerator'),
        nqp::mul_I(
          nqp::decont(b),
          nqp::getattr(nqp::decont(a),Rat,'$!denominator'),
          Int
        )
      )
    )
}
multi sub infix:«>=»(Int:D \a, Rational:D \b --> Bool:D) {
#    a * b.denominator >= b.numerator
    nqp::hllbool(
      nqp::isge_I(
        nqp::mul_I(
          nqp::decont(a),
          nqp::getattr(nqp::decont(b),Rat,'$!denominator'),
          Int
        ),
        nqp::getattr(nqp::decont(b),Rat,'$!numerator')
      )
    )
}

multi sub infix:«<=>»(Rational:D \a, Rational:D \b --> Order:D) {
#    a.numerator * b.denominator <=> b.numerator * a.denominator
    ORDER(
      nqp::cmp_I(
        nqp::mul_I(
          nqp::getattr(nqp::decont(a),Rat,'$!numerator'),
          nqp::getattr(nqp::decont(b),Rat,'$!denominator'),
          Int
        ),
        nqp::mul_I(
          nqp::getattr(nqp::decont(b),Rat,'$!numerator'),
          nqp::getattr(nqp::decont(a),Rat,'$!denominator'),
          Int
        )
      )
    )
}
multi sub infix:«<=>»(Rational:D \a, Int:D \b --> Order:D) {
#    a.numerator  <=> b * a.denominator
    ORDER(
      nqp::cmp_I(
        nqp::getattr(nqp::decont(a),Rat,'$!numerator'),
        nqp::mul_I(
          nqp::decont(b),
          nqp::getattr(nqp::decont(a),Rat,'$!denominator'),
          Int
        )
      )
    )
}
multi sub infix:«<=>»(Int:D \a, Rational:D \b --> Order:D) {
#    a * b.denominator <=> b.numerator
    ORDER(
      nqp::cmp_I(
        nqp::mul_I(
          nqp::decont(a),
          nqp::getattr(nqp::decont(b),Rat,'$!denominator'),
          Int
        ),
        nqp::getattr(nqp::decont(b),Rat,'$!numerator')
      )
    )
}

# vim: ft=perl6 expandtab sw=4
