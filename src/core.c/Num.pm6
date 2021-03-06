my class X::Cannot::Capture        { ... }
my class X::Numeric::DivideByZero  { ... }
my class X::Numeric::CannotConvert { ... }

my class Num does Real { # declared in BOOTSTRAP
    # class Num is Cool
    #     has num $!value is box_target;

    multi method WHICH(Num:D: --> ValueObjAt:D) {
        nqp::box_s(
          nqp::concat(
            nqp::if(
              nqp::eqaddr(self.WHAT,Num),
              'Num|',
              nqp::concat(nqp::unbox_s(self.^name), '|')
            ),
            nqp::unbox_n(self)
          ),
          ValueObjAt
        )
    }
    multi method Bool(Num:D:) { nqp::hllbool(nqp::isne_n(self,0e0)) }
    method Capture() { X::Cannot::Capture.new( :what(self) ).throw }
    method Num() { self }
    method Bridge(Num:) { self.defined ?? self !! self.Real::Bridge }
    method Range(Num:U:) { Range.new(-Inf,Inf) }

    method Int(Num:D:) {
        nqp::isnanorinf(nqp::unbox_n(self))
          ?? X::Numeric::CannotConvert.new(:source(self), :target(Int)).fail
          !! nqp::fromnum_I(nqp::unbox_n(self),Int)
    }

    method sign(Num:D:) {
        nqp::isnanorinf(self)
          ?? self == Inf ?? 1 !! self == -Inf ?? -1 !! NaN
          !! self  > 0   ?? 1 !! self <  0    ?? -1 !! 0
    }

    multi method new() { nqp::box_n(0e0, self) }
    multi method new($n) { nqp::box_n($n.Num, self) }

    multi method raku(Num:D:) {
        my str $res = self.Str;
        nqp::isnanorinf(nqp::unbox_n(self))
          || nqp::isge_i(nqp::index($res,'e'),0)
          || nqp::isge_i(nqp::index($res,'E'),0)
          ?? $res
          !! nqp::concat($res,'e0')
    }

    method Rat(Num:D: Real:D \epsilon = 1.0e-6, \RAT = Rat) {
        my num $num = self;

        return RAT.new(
          (nqp::iseq_n($num,$num) ?? nqp::iseq_n($num,Inf) ?? 1 !! -1 !! 0),
          0
        ) if nqp::isnanorinf($num);

        $num = nqp::neg_n($num) if (my int $signum = nqp::islt_n($num,0e0));
        my num $r = nqp::sub_n($num,nqp::floor_n($num));

        # basically have an Int
        if nqp::iseq_n($r,0e0) {
            RAT.new(nqp::fromnum_I(self,Int),1)
        }

        # find convergents of the continued fraction.
        else {
            my Int $a := 1;
            my Int $b := nqp::fromnum_I($num,Int);
            my Int $c := 0;
            my Int $d := 1;

            # bind some value to prevent Scalar container creation
            my Int $q      := 0;
            my Int $orig_b := 0;
            my Int $orig_d := 0;

            my num $modf_arg;
            my num $epsilon = epsilon.Num;

            nqp::while(
              nqp::isne_n($r,0e0)
                && nqp::isgt_n(
                     nqp::abs_n(nqp::sub_n($num,nqp::div_In($b,$d))),
                     $epsilon
                   ),
              nqp::stmts(
                ($modf_arg = nqp::div_n(1e0,$r)),
                ($q := nqp::fromnum_I($modf_arg,Int)),
                ($r  = nqp::sub_n($modf_arg,nqp::floor_n($modf_arg))),

                ($orig_b := $b),
                ($b := nqp::add_I(nqp::mul_I($q,$b,Int),$a,Int)),
                ($a := $orig_b),

                ($orig_d := $d),
                ($d := nqp::add_I(nqp::mul_I($q,$d,Int),$c,Int)),
                ($c := $orig_d)
              )
            );

            # Note that this result has less error than any Rational with a
            # smaller denominator but it is not (necessarily) the Rational
            # with the smallest denominator that has less than $epsilon error.
            # However, to find that Rational would take more processing.
            RAT.new($signum ?? nqp::neg_I($b,Int) !! $b, $d)
        }
    }
    method FatRat(Num:D: Real $epsilon = 1.0e-6) {
        self.Rat($epsilon, FatRat);
    }

    multi method atan2(Num:D: Num:D $x = 1e0) {
        nqp::p6box_n(nqp::atan2_n(nqp::unbox_n(self), nqp::unbox_n($x)));
    }

    multi method Str(Num:D:) {
        nqp::p6box_s(nqp::unbox_n(self));
    }

    method succ(Num:D:) { self + 1e0 }

    method pred(Num:D:) { self - 1e0 }

    method isNaN(Num:D: ) {
        self != self;
    }

    method abs(Num:D: ) {
        nqp::p6box_n(nqp::abs_n(nqp::unbox_n(self)));
    }

    multi method exp(Num:D: ) {
        nqp::p6box_n(nqp::exp_n(nqp::unbox_n(self)));
    }

    proto method log(|) {*}
    multi method log(Num:D: ) {
        nqp::p6box_n(nqp::log_n(nqp::unbox_n(self)));
    }
    multi method log(Num:D: Num \base) {
        self.log() / base.log();
    }

    proto method sqrt(|) {*}
    multi method sqrt(Num:D: ) {
        nqp::p6box_n(nqp::sqrt_n(nqp::unbox_n(self)));
    }

    method rand(Num:D: ) {
        nqp::p6box_n(nqp::rand_n(nqp::unbox_n(self)));
    }

    method ceiling(Num:D: ) {
        nqp::isnanorinf(nqp::unbox_n(self))
            ?? self
            !! nqp::fromnum_I(nqp::ceil_n(nqp::unbox_n(self)), Int);
    }
    method floor(Num:D: ) {
        nqp::isnanorinf(nqp::unbox_n(self))
            ?? self
            !! nqp::fromnum_I(nqp::floor_n(nqp::unbox_n(self)), Int);
    }

    proto method sin(|) {*}
    multi method sin(Num:D: ) {
        nqp::p6box_n(nqp::sin_n(nqp::unbox_n(self)));
    }
    proto method asin(|) {*}
    multi method asin(Num:D: ) {
        nqp::p6box_n(nqp::asin_n(nqp::unbox_n(self)));
    }
    proto method cos(|) {*}
    multi method cos(Num:D: ) {
        nqp::p6box_n(nqp::cos_n(nqp::unbox_n(self)));
    }
    proto method acos(|) {*}
    multi method acos(Num:D: ) {
        nqp::p6box_n(nqp::acos_n(nqp::unbox_n(self)));
    }
    proto method tan(|) {*}
    multi method tan(Num:D: ) {
        nqp::p6box_n(nqp::tan_n(nqp::unbox_n(self)));
    }
    proto method atan(|) {*}
    multi method atan(Num:D: ) {
        nqp::p6box_n(nqp::atan_n(nqp::unbox_n(self)));
    }
    proto method sec(|) {*}
    multi method sec(Num:D: ) {
        nqp::p6box_n(nqp::div_n(1e0, nqp::cos_n(nqp::unbox_n(self))));
    }
    proto method asec(|) {*}
    multi method asec(Num:D: ) {
        nqp::p6box_n(nqp::acos_n(nqp::div_n(1e0, nqp::unbox_n(self))));
    }
    method cosec(Num:D:) {
        nqp::p6box_n(nqp::div_n(1e0, nqp::sin_n(nqp::unbox_n(self))));
    }
    method acosec(Num:D:) {
        nqp::p6box_n(nqp::asin_n(nqp::div_n(1e0, nqp::unbox_n(self))));
    }
    method cotan(Num:D:) {
        nqp::p6box_n(nqp::div_n(1e0, nqp::tan_n(nqp::unbox_n(self))));
    }
    method acotan(Num:D:) {
        nqp::p6box_n(nqp::atan_n(nqp::div_n(1e0, nqp::unbox_n(self))));
    }
    proto method sinh(|) {*}
    multi method sinh(Num:D: ) {
        nqp::p6box_n(nqp::sinh_n(nqp::unbox_n(self)));
    }
    proto method asinh(|) {*}
    multi method asinh(Num:D: ) {
        nqp::isnanorinf(self)
            ?? self
            !!
                self >= 0
                    ?? (self + (self * self + 1e0).sqrt).log
                    !! -(-1e0 * self).asinh
    }
    proto method cosh(|) {*}
    multi method cosh(Num:D: ) {
        nqp::p6box_n(nqp::cosh_n(nqp::unbox_n(self)));
    }
    proto method acosh(|) {*}
    multi method acosh(Num:D: ) {
        self < 1e0
            ?? NaN
            !! (self + (self * self - 1e0).sqrt).log;
    }
    proto method tanh(|) {*}
    multi method tanh(Num:D: ) {
        nqp::p6box_n(nqp::tanh_n(nqp::unbox_n(self)));
    }
    proto method atanh(|) {*}
    multi method atanh(1e0:) { ∞ }
    multi method atanh(Num:D: ) {
        ((1e0 + self) / (1e0 - self)).log / 2e0;
    }
    proto method sech(|) {*}
    multi method sech(Num:D: ) {
        nqp::p6box_n(nqp::div_n(1e0, nqp::cosh_n(nqp::unbox_n(self))));
    }
    proto method asech(|) {*}
    multi method asech(Num:D: ) {
        (1e0 / self).acosh;
    }
    proto method cosech(|) {*}
    multi method cosech(Num:D: ) {
        nqp::p6box_n(nqp::div_n(1e0, nqp::sinh_n(nqp::unbox_n(self))));
    }
    proto method acosech(|) {*}
    multi method acosech(Num:D: ) {
        (1e0 / self).asinh;
    }
    proto method cotanh(|) {*}
    multi method cotanh(Num:D: ) {
        nqp::p6box_n(nqp::div_n(1e0, nqp::tanh_n(nqp::unbox_n(self))));
    }
    proto method acotanh(|) {*}
    multi method acotanh(Num:D: ) {
        (1e0 / self).atanh;
    }
    method is-prime(--> Bool:D) {
        nqp::hllbool(
          nqp::if(
            nqp::isnanorinf(self),
            False,
            nqp::if(
              nqp::iseq_n(self,nqp::floor_n(self)),
              nqp::fromnum_I(self,Int).is-prime
            )
          )
        )
    }

    method narrow(Num:D:) {
        my $i := self.Int;
        $i.defined && $i.Num ≅ self
            ?? $i
            !! self
    }
}

my constant tau = 6.28318_53071_79586_476e0;
my constant pi  = 3.14159_26535_89793_238e0;
my constant e   = 2.71828_18284_59045_235e0;

my constant π := pi;
my constant τ := tau;
#?if !jvm
my constant 𝑒 := e;
#?endif

multi sub prefix:<++>(Num:D $a is rw) {
    $a = nqp::p6box_n(nqp::add_n(nqp::unbox_n($a), 1e0))
}
multi sub prefix:<++>(Num:U $a is rw) {
    $a = 1e0;
}
multi sub prefix:<++>(num $a is rw --> num) {
    $a = nqp::add_n($a, 1e0)
}
multi sub prefix:<-->(Num:D $a is rw) {
    $a = nqp::p6box_n(nqp::sub_n(nqp::unbox_n($a), 1e0))
}
multi sub prefix:<-->(Num:U $a is rw) {
    $a = -1e0;
}
multi sub prefix:<-->(num $a is rw --> num) {
    $a = nqp::sub_n($a, 1e0)
}
multi sub postfix:<++>(Num:D $a is rw) {
    my $b = $a;
    $a = nqp::p6box_n(nqp::add_n(nqp::unbox_n($a), 1e0));
    $b
}
multi sub postfix:<++>(Num:U $a is rw --> 0e0) {
    $a = 1e0;
}
multi sub postfix:<++>(num $a is rw --> num) {
    my num $b = $a;
    $a = nqp::add_n($a, 1e0);
    $b
}
multi sub postfix:<-->(Num:D $a is rw) {
    my $b = $a;
    $a = nqp::p6box_n(nqp::sub_n(nqp::unbox_n($a), 1e0));
    $b
}
multi sub postfix:<-->(Num:U $a is rw --> 0e0) {
    $a = -1e0;
}
multi sub postfix:<-->(num $a is rw --> num) {
    my num $b = $a;
    $a = nqp::sub_n($a, 1e0);
    $b
}

multi sub prefix:<->(Num:D \a) {
    nqp::p6box_n(nqp::neg_n(nqp::unbox_n(a)))
}
multi sub prefix:<->(num $a --> num) {
    nqp::neg_n($a);
}

multi sub abs(Num:D \a) {
    nqp::p6box_n(nqp::abs_n(nqp::unbox_n(a)))
}
multi sub abs(num $a --> num) {
    nqp::abs_n($a)
}

multi sub infix:<+>(Num:D \a, Num:D \b) {
    nqp::p6box_n(nqp::add_n(nqp::unbox_n(a), nqp::unbox_n(b)))
}
multi sub infix:<+>(num $a, num $b --> num) {
    nqp::add_n($a, $b)
}

multi sub infix:<->(Num:D \a, Num:D \b) {
    nqp::p6box_n(nqp::sub_n(nqp::unbox_n(a), nqp::unbox_n(b)))
}
multi sub infix:<->(num $a, num $b --> num) {
    nqp::sub_n($a, $b)
}

multi sub infix:<*>(Num:D \a, Num:D \b) {
    nqp::p6box_n(nqp::mul_n(nqp::unbox_n(a), nqp::unbox_n(b)))
}
multi sub infix:<*>(num $a, num $b --> num) {
    nqp::mul_n($a, $b)
}

multi sub infix:</>(Num:D \a, Num:D \b) {
    b
      ?? nqp::p6box_n(nqp::div_n(nqp::unbox_n(a), nqp::unbox_n(b)))
      !! Failure.new(X::Numeric::DivideByZero.new(:using</>, :numerator(a)))
}
multi sub infix:</>(num $a, num $b --> num) {
    $b
      ?? nqp::div_n($a, $b)
      !! Failure.new(X::Numeric::DivideByZero.new(:using</>, :numerator($a)))
}

multi sub infix:<%>(Num:D \a, Num:D \b) {
    b
      ?? nqp::p6box_n(nqp::mod_n(nqp::unbox_n(a), nqp::unbox_n(b)))
      !! Failure.new(X::Numeric::DivideByZero.new(:using<%>, :numerator(a)))
}
multi sub infix:<%>(num $a, num $b --> num) {
    $b
      ?? nqp::mod_n($a, $b)
      !! Failure.new(X::Numeric::DivideByZero.new(:using<%>, :numerator($a)))
}

# (If we get 0 here, must be underflow, since floating overflow provides Inf.)
multi sub infix:<**>(Num:D \a, Num:D \b) {
    nqp::p6box_n(nqp::pow_n(nqp::unbox_n(a), nqp::unbox_n(b)))
      or a == 0e0 || b.abs == Inf
        ?? 0e0
        !! Failure.new(X::Numeric::Underflow.new)
}
multi sub infix:<**>(num $a, num $b --> num) {
    nqp::pow_n($a, $b)
      or $a == 0e0 || $b.abs == Inf
        ?? 0e0
        !! Failure.new(X::Numeric::Underflow.new)
}

# Here we sort NaN in with string "NaN"
multi sub infix:<cmp>(Num:D \a, Num:D \b) {
    nqp::cmp_n(nqp::unbox_n(a), nqp::unbox_n(b)) ?? ORDER(nqp::cmp_n(nqp::unbox_n(a), nqp::unbox_n(b)))
       !! a === b ?? Same # === cares about signed zeros, we don't, so:
            !! nqp::iseq_n(a, 0e0) && nqp::iseq_n(b, 0e0)
                ?? Same !! a.Stringy cmp b.Stringy;
}
multi sub infix:<cmp>(num $a, num $b) {
    nqp::cmp_n($a, $b) ?? ORDER(nqp::cmp_n($a, $b))
       !! $a === $b ?? Same # === cares about signed zeros, we don't, so:
            !! nqp::iseq_n($a, 0e0) && nqp::iseq_n($b, 0e0)
                ?? Same !! $a.Stringy cmp $b.Stringy;
}

# Here we treat NaN as undefined
multi sub infix:«<=>»(Num:D \a, Num:D \b) {
    ORDER(nqp::cmp_n(nqp::unbox_n(a), nqp::unbox_n(b))) or
         a == b ?? Same !! Nil;
}
multi sub infix:«<=>»(num $a, num $b) {
    ORDER(nqp::cmp_n($a, $b)) or
         $a == $b ?? Same !! Nil;
}

multi sub infix:<===>(Num:D \a, Num:D \b) {
    nqp::hllbool(
        nqp::eqaddr(a.WHAT,b.WHAT)
        && (
            ( # Both are NaNs
                   nqp::not_i(nqp::isle_n(a, nqp::inf))
                && nqp::not_i(nqp::isle_n(b, nqp::inf))
            )
            || (
                nqp::iseq_n(a, b)
                && ( # if we're dealing with zeros, ensure the signs match
                    nqp::isne_n(a, 0e0)
                    || nqp::if( # 1/-0 = -Inf; 1/0 = +Inf
                        nqp::islt_n(nqp::div_n(1e0,a), 0e0), # a is -0, if true:
                        nqp::islt_n(nqp::div_n(1e0,b), 0e0), #   check b is -0 too
                        nqp::isgt_n(nqp::div_n(1e0,b), 0e0), #   check b is +0 too
                    )
                )
            )
        )
    )
}
multi sub infix:<===>(num \a, num \b --> Bool:D) {
    nqp::hllbool(
        nqp::eqaddr(a.WHAT,b.WHAT)
        && (
            ( # Both are NaNs
                   nqp::not_i(nqp::isle_n(a, nqp::inf))
                && nqp::not_i(nqp::isle_n(b, nqp::inf))
            )
            || (
                nqp::iseq_n(a, b)
                && ( # if we're dealing with zeros, ensure the signs match
                    nqp::isne_n(a, 0e0)
                    || nqp::if( # 1/-0 = -Inf; 1/0 = +Inf
                        nqp::islt_n(nqp::div_n(1e0,a), 0e0), # a is -0, if true:
                        nqp::islt_n(nqp::div_n(1e0,b), 0e0), #   check b is -0 too
                        nqp::isgt_n(nqp::div_n(1e0,b), 0e0), #   check b is +0 too
                    )
                )
            )
        )
    )
}

multi sub infix:<≅>( Inf,  Inf) { Bool::True }
multi sub infix:<≅>(-Inf, -Inf) { Bool::True }

multi sub infix:<==>(Num:D \a, Num:D \b --> Bool:D)  {
    nqp::hllbool(nqp::iseq_n(nqp::unbox_n(a), nqp::unbox_n(b)))
}
multi sub infix:<==>(num $a, num $b --> Bool:D)  {
    nqp::hllbool(nqp::iseq_n($a, $b))
}

multi sub infix:<!=>(num $a, num $b --> Bool:D) {
    nqp::hllbool(nqp::isne_n($a, $b))
}

multi sub infix:«<»(Num:D \a, Num:D \b --> Bool:D) {
    nqp::hllbool(nqp::islt_n(nqp::unbox_n(a), nqp::unbox_n(b)))
}
multi sub infix:«<»(num $a, num $b --> Bool:D) {
    nqp::hllbool(nqp::islt_n($a, $b))
}

multi sub infix:«<=»(Num:D \a, Num:D \b --> Bool:D) {
    nqp::hllbool(nqp::isle_n(nqp::unbox_n(a), nqp::unbox_n(b)))
}
multi sub infix:«<=»(num $a, num $b --> Bool:D) {
    nqp::hllbool(nqp::isle_n($a, $b))
}

multi sub infix:«>»(Num:D \a, Num:D \b --> Bool:D) {
    nqp::hllbool(nqp::isgt_n(nqp::unbox_n(a), nqp::unbox_n(b)))
}
multi sub infix:«>»(num $a, num $b --> Bool:D) {
    nqp::hllbool(nqp::isgt_n($a, $b))
}

multi sub infix:«>=»(Num:D \a, Num:D \b --> Bool:D) {
    nqp::hllbool(nqp::isge_n(nqp::unbox_n(a), nqp::unbox_n(b)))
}
multi sub infix:«>=»(num $a, num $b --> Bool:D) {
    nqp::hllbool(nqp::isge_n($a, $b))
}

proto sub rand(*%) {*}
multi sub rand(--> Num:D) { nqp::p6box_n(nqp::rand_n(1e0)) }

proto sub srand($, *%) {*}
multi sub srand(Int:D $seed --> Int:D) { nqp::p6box_i(nqp::srand($seed)) }

multi sub atan2(Num:D $a, Num:D $b = 1e0) {
    nqp::p6box_n(nqp::atan2_n(nqp::unbox_n($a), nqp::unbox_n($b)));
}

multi sub cosec(Num:D \x) {
    nqp::p6box_n(nqp::div_n(1e0, nqp::sin_n(nqp::unbox_n(x))));
}
multi sub acosec(Num:D \x) {
    nqp::p6box_n(nqp::asin_n(nqp::div_n(1e0, nqp::unbox_n(x))));
}

multi sub log(num $x --> num) {
    nqp::log_n($x);
}

multi sub sin(num $x --> num) {
    nqp::sin_n($x);
}
multi sub asin(num $x --> num) {
    nqp::asin_n($x);
}
multi sub cos(num $x --> num) {
    nqp::cos_n($x);
}
multi sub acos(num $x --> num) {
    nqp::acos_n($x);
}
multi sub tan(num $x --> num) {
    nqp::tan_n($x);
}
multi sub atan(num $x --> num) {
    nqp::atan_n($x);
}
multi sub sec(num $x --> num) {
    nqp::div_n(1e0, nqp::cos_n($x));
}
multi sub asec(num $x --> num) {
    nqp::acos_n(nqp::div_n(1e0, $x));
}

multi sub cotan(num $x --> num) {
    nqp::div_n(1e0, nqp::tan_n($x));
}
multi sub acotan(num $x --> num) {
    nqp::atan_n(nqp::div_n(1e0, $x));
}
multi sub sinh(num $x --> num) {
    nqp::sinh_n($x);
}
multi sub asinh(num $x --> num) {
    # ln(x + √(x²+1))
    nqp::isnanorinf($x)
        ?? $x
        !! $x >= 0
            ?? nqp::log_n(
                nqp::add_n(
                    $x,
                    nqp::pow_n( nqp::add_n(nqp::mul_n($x,$x), 1e0), .5e0 )
                )
            )
            !! -asinh(-$x);
}

multi sub cosh(num $x --> num) {
    nqp::cosh_n($x);
}
multi sub acosh(num $x --> num) {
    # ln(x + √(x²-1))
    $x < 1e0
        ?? NaN
        !! nqp::log_n(
            nqp::add_n(
                $x,
                nqp::pow_n( nqp::sub_n(nqp::mul_n($x,$x), 1e0), .5e0 )
            )
        )
}
multi sub tanh(num $x --> num) {
    nqp::tanh_n($x);
}
multi sub atanh(num $x --> num) {
    $x == 1e0 ?? Inf !! log((1e0 + $x) / (1e0 - $x)) / 2e0;
}
multi sub sech(num $x --> num) {
    1e0 / cosh($x)
}
multi sub asech(num $x --> num) {
    acosh(1e0 / $x);
}
multi sub cosech(num $x --> num) {
    1e0 / sinh($x)
}
multi sub acosech(num $x --> num) {
    asinh(1e0 / $x);
}
multi sub cotanh(num $x --> num) {
    1e0 / tanh($x);
}
multi sub acotanh(num $x --> num) {
    atanh(1e0 / $x)
}

multi sub floor(num $a --> num) {
    nqp::floor_n($a)
}
multi sub ceiling(num $a --> num) {
    nqp::ceil_n($a)
}
multi sub sqrt(num $a --> num) {
    nqp::sqrt_n($a)
}

# vim: expandtab shiftwidth=4
