my class Raku { … }

# XXX: should be Rational[Int, uint]
my class Rat is Cool does Rational[Int, Int] {
    method Rat(Rat:D: Real $? --> Rat:D) {
        self
    }
    method FatRat(Rat:D: Real $? --> FatRat:D) {
        nqp::p6bindattrinvres(
          nqp::p6bindattrinvres(
            nqp::create(FatRat),FatRat,'$!numerator',$!numerator
          ),
          FatRat,'$!denominator',$!denominator
        )
    }
    multi method raku(Rat:D: --> Str:D) {
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
            elsif Raku.legacy {
                '<' ~ $!numerator ~ '/' ~ $!denominator ~ '>'
            }
            else {
                if $!numerator div $!denominator -> $wholes {
                    my $parts := $!numerator - ($!denominator * $wholes);
                    "$wholes$parts.Str(:superscript)/$!denominator.Str(:subscript)"
                }
                else {
                    "$!numerator.Str(:superscript)/$!denominator.Str(:subscript)"
                }
            }
        }
    }
}

my constant UINT64_UPPER = nqp::pow_I(2, 64, Num, Int);

my class FatRat is Cool does Rational[Int, Int] {
    method FatRat(FatRat:D:) { self }
    method Rat(FatRat:D:) {
        $!denominator < UINT64_UPPER
          ?? nqp::p6bindattrinvres(
               nqp::p6bindattrinvres(
                 nqp::create(Rat),Rat,'$!numerator',$!numerator
               ),
               Rat,'$!denominator',$!denominator
             )
          !! "Cannot convert from FatRat to Rat because denominator is too big".Failure
    }
    multi method raku(FatRat:D: --> Str:D) {
        (nqp::eqaddr(self.WHAT,FatRat) ?? 'FatRat' !! self.^name)
          ~ ".new($!numerator, $!denominator)"
    }

    method UPGRADE-RAT(\nu, \de) is raw {
        nqp::p6bindattrinvres(
          nqp::p6bindattrinvres(nqp::create(FatRat),FatRat,'$!numerator',nu),
          FatRat,'$!denominator',de
        )
    }
}

# NORMALIZE two integer values and create a Rat/FatRat/float from them.
# Provide two types: if either of them is a FatRat, then a FatRat will be
# returned.  If a Rat is to be created, then a check for denominator overflow
# is done: if that is the case, then a float will be returned.
sub DIVIDE_NUMBERS(
  Int:D $nu, Int:D $de, \t1, \t2
) is raw is implementation-detail {
    nqp::if(
      $de,
      nqp::stmts(
        (my \gcd         := nqp::gcd_I($nu,$de,Int)),
        (my \numerator   := nqp::div_I($nu,gcd,Int)),
        (my \denominator := nqp::div_I($de,gcd,Int)),
        nqp::if(
          nqp::islt_I(denominator,0),
          CREATE_RATIONAL_FROM_INTS(
            nqp::neg_I(numerator,Int), nqp::neg_I(denominator,Int), t1, t2
          ),
          CREATE_RATIONAL_FROM_INTS(
            numerator, denominator, t1, t2
          )
        )
      ),
      CREATE_RATIONAL_FROM_INTS(
        nqp::box_i(nqp::isgt_I($nu,0) || nqp::neg_i(nqp::istrue($nu)),Int),
        0, t1, t2
      )
    )
}

# Initialize the $*RAT-OVERFLOW dynamic var so that it can be used
PROCESS::<$RAT-OVERFLOW> = Num;

# ALL RATIONALS MUST BE NORMALIZED, however in some operations we cannot
# ever get a non-normalized Rational, if we start with a normalized Rational.
# For such cases, we can use this routine, to bypass normalization step,
# which would be useless.  Also used when normalization *was* needed.
proto sub CREATE_RATIONAL_FROM_INTS(|) is implementation-detail {*}
multi sub CREATE_RATIONAL_FROM_INTS(Int:D $nu, Int:D $de, Any, Any) is raw {
    nqp::islt_I($de,UINT64_UPPER)            # do we need to downgrade to float?
      ?? nqp::p6bindattrinvres(              # no, we need to keep a Rat
           nqp::p6bindattrinvres(nqp::create(Rat),Rat,'$!numerator',$nu),
           Rat,'$!denominator',$de
         )
      !! $*RAT-OVERFLOW.UPGRADE-RAT($nu, $de)
}

# already a FatRat, so keep that
multi sub CREATE_RATIONAL_FROM_INTS(
  Int:D $nu, Int:D $de, FatRat, Any
--> FatRat:D) is raw {
    nqp::p6bindattrinvres(
      nqp::p6bindattrinvres(nqp::create(FatRat),FatRat,'$!numerator',$nu),
      FatRat,'$!denominator',$de
    )
}
multi sub CREATE_RATIONAL_FROM_INTS(
  Int:D $nu, Int:D $de, Any, FatRat
--> FatRat:D) is raw {
    nqp::p6bindattrinvres(
      nqp::p6bindattrinvres(nqp::create(FatRat),FatRat,'$!numerator',$nu),
      FatRat,'$!denominator',$de
    )
}
multi sub CREATE_RATIONAL_FROM_INTS(
  Int:D $nu, Int:D $de, FatRat, FatRat
--> FatRat:D) is raw {
    nqp::p6bindattrinvres(
      nqp::p6bindattrinvres(nqp::create(FatRat),FatRat,'$!numerator',$nu),
      FatRat,'$!denominator',$de
    )
}

multi sub prefix:<->(Rat:D $a --> Rat:D) {
#    Rat.new(-a.numerator, a.denominator);
    nqp::p6bindattrinvres(
      nqp::clone($a),
      Rat,'$!numerator',nqp::neg_I($a.numerator,Int)
    )
}
multi sub prefix:<->(FatRat:D $a --> FatRat:D) {
#    FatRat.new(-a.numerator, a.denominator);
    nqp::p6bindattrinvres(
      nqp::clone($a),
      FatRat,'$!numerator',nqp::neg_I($a.numerator,Int)
    )
}

multi sub infix:<+>(Rational:D $a, Rational:D $b) {
    my \adenom := $a.denominator;
    my \bdenom := $b.denominator;
    DIVIDE_NUMBERS(
      $a.numerator * bdenom + $b.numerator * adenom,
      adenom * bdenom, $a, $b
    )
}
multi sub infix:<+>(Rational:D $a, Int:D $b) {
    my \adenom := $a.denominator;
    DIVIDE_NUMBERS(
      $a.numerator + $b * adenom,
      adenom, $a, $b
    )
}
multi sub infix:<+>(Int:D $a, Rational:D $b) {
    my \bdenom := $b.denominator;
    DIVIDE_NUMBERS(
      $a * bdenom + $b.numerator,
      bdenom, $a, $b
    )
}

multi sub infix:<->(Rational:D $a, Rational:D $b) {
    my \adenom := $a.denominator;
    my \bdenom := $b.denominator;
    DIVIDE_NUMBERS(
      $a.numerator * bdenom - $b.numerator * adenom,
      adenom * bdenom, $a, $b
    )
}

multi sub infix:<->(Rational:D $a, Int:D $b) {
    my \adenom := $a.denominator;
    DIVIDE_NUMBERS(
      $a.numerator - $b * adenom,
      adenom, $a, $b
    )
}

multi sub infix:<->(Int:D $a, Rational:D $b) {
    my \bdenom := $b.denominator;
    DIVIDE_NUMBERS(
      $a * bdenom - $b.numerator,
      bdenom, $a, $b
    )
}

multi sub infix:<*>(Rational:D $a, Rational:D $b) {
    DIVIDE_NUMBERS(
      $a.numerator * $b.numerator,
      $a.denominator * $b.denominator,
      $a, $b
    )
}

multi sub infix:<*>(Rational:D $a, Int:D $b) {
    DIVIDE_NUMBERS(
      $a.numerator * $b,
      $a.denominator,
      $a, $b
    )
}

multi sub infix:<*>(Int:D $a, Rational:D $b) {
    DIVIDE_NUMBERS(
      $a * $b.numerator,
      $b.denominator,
      $a, $b
    )
}

multi sub infix:</>(Rational:D $a, Rational:D $b) {
    DIVIDE_NUMBERS(
      $a.numerator * $b.denominator,
      $a.denominator * $b.numerator,
      $a, $b
    )
}

multi sub infix:</>(Rational:D $a, Int:D $b) {
    DIVIDE_NUMBERS(
      $a.numerator,
      $a.denominator * $b,
      $a, $b
    )
}

multi sub infix:</>(Int:D $a, Rational:D $b) {
    DIVIDE_NUMBERS(
      $b.denominator * $a,
      $b.numerator,
      $a, $b
    )
}

multi sub infix:</>(Int:D $a, Int:D $b) {
    DIVIDE_NUMBERS($a, $b, $a, $b)
}

multi sub infix:<%>(Rational:D $a, Int:D $b) {
    $a - floor($a / $b) * $b
}

multi sub infix:<%>(Int:D $a, Rational:D $b) {
    $a - floor($a / $b) * $b
}

multi sub infix:<%>(Rational:D $a, Rational:D $b) {
    $a - floor($a / $b) * $b
}

multi sub infix:<**>(Rational:D $a, Int:D $b) {
    my $nu;
    my $de;
    nqp::if(
      nqp::isge_I($b,0),
      nqp::if( # if we got Inf
        nqp::istype(
          ($nu := nqp::pow_I($a.numerator,$b,Num,Int)),
          Num
        ),
        X::Numeric::Overflow.new.Failure,
        nqp::if( # if we got Inf
          nqp::istype(
            ($de := nqp::pow_I($a.denominator,$b,Num,Int)),
            Num
          ),
          X::Numeric::Overflow.new.Failure,
          CREATE_RATIONAL_FROM_INTS($nu, $de, $a, $b)
        )
      ),
      nqp::if( # if we got Inf
        nqp::istype(
          ($nu := nqp::pow_I($a.numerator,nqp::neg_I($b,Int),Num,Int)),
          Num
        ),
        X::Numeric::Underflow.new.Failure,
        nqp::if( # if we got Inf
          nqp::istype(
            ($de := nqp::pow_I($a.denominator,nqp::neg_I($b,Int),Num,Int)),
            Num
          ),
          X::Numeric::Underflow.new.Failure,
          CREATE_RATIONAL_FROM_INTS($de, $nu, $a, $b)
        )
      )
    )
}

multi sub infix:<==>(Rational:D $a, Rational:D $b --> Bool:D) {
    nqp::hllbool(
      nqp::iseq_I(
        (my \anum := $a.numerator),
        $b.numerator
      ) && nqp::iseq_I(
             (my \adenom := $a.denominator),
             $b.denominator
          ) && (                         # num/denom both same
                 nqp::istrue(anum)       # 1/X, Inf == Inf also true
                 || nqp::istrue(adenom)  # 0/1, NaN == NaN becomes false
               )
    )
}
multi sub infix:<==>(Rational:D $a, Int:D $b --> Bool:D) {
    nqp::hllbool(
      nqp::iseq_I($a.denominator,1)
        && nqp::iseq_I($a.numerator,$b)
    )
}
multi sub infix:<==>(Int:D $a, Rational:D $b --> Bool:D) {
    nqp::hllbool(
      nqp::iseq_I($b.denominator,1)
        && nqp::iseq_I($a,$b.numerator)
    )
}
multi sub infix:<===>(Rational:D $a, Rational:D $b --> Bool:D) {
    nqp::hllbool(
      nqp::eqaddr($a.WHAT, $b.WHAT)
        && nqp::iseq_I($a.numerator,  $b.numerator)
        && nqp::iseq_I($a.denominator,$b.denominator)
    )
}

multi sub infix:«<»(Rational:D $a, Rational:D $b --> Bool:D) {
#    a.numerator * b.denominator < b.numerator * a.denominator
    nqp::hllbool(
      nqp::islt_I(
        nqp::mul_I($a.numerator,$b.denominator,Int),
        nqp::mul_I($b.numerator,$a.denominator,Int)
      )
    )
}
multi sub infix:«<»(Rational:D $a, Int:D $b --> Bool:D) {
#    a.numerator  < b * a.denominator
    nqp::hllbool(
      nqp::islt_I($a.numerator,nqp::mul_I($b,$a.denominator,Int))
    )
}
multi sub infix:«<»(Int:D $a, Rational:D $b --> Bool:D) {
#    a * b.denominator < b.numerator
    nqp::hllbool(
      nqp::islt_I(nqp::mul_I($a,$b.denominator,Int),$b.numerator)
    )
}

multi sub infix:«<=»(Rational:D $a, Rational:D $b --> Bool:D) {
#    a.numerator * b.denominator <= b.numerator * a.denominator
    nqp::hllbool(
      nqp::isle_I(
        nqp::mul_I($a.numerator,$b.denominator,Int),
        nqp::mul_I($b.numerator,$a.denominator,Int)
      )
    )
}
multi sub infix:«<=»(Rational:D $a, Int:D $b --> Bool:D) {
#    a.numerator  <= b * a.denominator
    nqp::hllbool(
      nqp::isle_I($a.numerator,nqp::mul_I($b,$a.denominator,Int))
    )
}
multi sub infix:«<=»(Int:D $a, Rational:D $b --> Bool:D) {
#    a * b.denominator <= b.numerator
    nqp::hllbool(
      nqp::isle_I(nqp::mul_I($a,$b.denominator,Int),$b.numerator)
    )
}

multi sub infix:«>»(Rational:D $a, Rational:D $b --> Bool:D) {
#    a.numerator * b.denominator > b.numerator * a.denominator
    nqp::hllbool(
      nqp::isgt_I(
        nqp::mul_I($a.numerator,$b.denominator,Int),
        nqp::mul_I($b.numerator,$a.denominator,Int)
      )
    )
}
multi sub infix:«>»(Rational:D $a, Int:D $b --> Bool:D) {
#    a.numerator  > b * a.denominator
    nqp::hllbool(
      nqp::isgt_I($a.numerator,nqp::mul_I($b,$a.denominator,Int))
    )
}
multi sub infix:«>»(Int:D $a, Rational:D $b --> Bool:D) {
#    a * b.denominator > b.numerator
    nqp::hllbool(
      nqp::isgt_I(nqp::mul_I($a,$b.denominator,Int),$b.numerator)
    )
}

multi sub infix:«>=»(Rational:D $a, Rational:D $b --> Bool:D) {
#    a.numerator * b.denominator >= b.numerator * a.denominator
    nqp::hllbool(
      nqp::isge_I(
        nqp::mul_I($a.numerator,$b.denominator,Int),
        nqp::mul_I($b.numerator,$a.denominator,Int)
      )
    )
}
multi sub infix:«>=»(Rational:D $a, Int:D $b --> Bool:D) {
#    a.numerator  >= b * a.denominator
    nqp::hllbool(
      nqp::isge_I($a.numerator,nqp::mul_I($b,$a.denominator,Int))
    )
}
multi sub infix:«>=»(Int:D $a, Rational:D $b --> Bool:D) {
#    a * b.denominator >= b.numerator
    nqp::hllbool(
      nqp::isge_I(nqp::mul_I($a,$b.denominator,Int),$b.numerator)
    )
}

multi sub infix:«<=>»(Rational:D $a, Rational:D $b) {
#    a.numerator * b.denominator <=> b.numerator * a.denominator
    my $na := $a.numerator;
    my $nb := $b.numerator;
    my $da := $a.denominator;
    my $db := $b.denominator;

    ORDER(
      $da == $db
        ?? nqp::cmp_I($na,$nb)
        !! nqp::cmp_I(
             nqp::mul_I($na,$db,Int),
             nqp::mul_I($nb,$da,Int)
           )
    )
}
multi sub infix:«<=>»(Rational:D $a, Int:D $b) {
#    a.numerator  <=> b * a.denominator
    ORDER(
      nqp::cmp_I($a.numerator,nqp::mul_I($b,$a.denominator,Int))
    )
}
multi sub infix:«<=>»(Int:D $a, Rational:D $b) {
#    a * b.denominator <=> b.numerator
    ORDER(
      nqp::cmp_I(nqp::mul_I($a,$b.denominator,Int),$b.numerator)
    )
}

# vim: expandtab shiftwidth=4
