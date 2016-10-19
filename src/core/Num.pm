my class X::Numeric::DivideByZero { ... };
my role Rational { ... };

my class Num does Real { # declared in BOOTSTRAP
    # class Num is Cool {
    #     has num $!value is box_target;

    multi method WHICH(Num:D:) {
        nqp::box_s(
            nqp::concat(
                nqp::concat(nqp::unbox_s(self.^name), '|'),
                nqp::unbox_n(self)
            ),
            ObjAt
        );
    }
    method Num() { self }
    method Bridge(Num:D:) { self }
    method Range(Num:U:) { Range.new(-Inf,Inf) }

    method Int(Num:D:) {
        nqp::isnanorinf(nqp::unbox_n(self))
          ?? Failure.new("Cannot coerce {self} to an Int")
          !! nqp::fromnum_I(nqp::unbox_n(self),Int)
    }

    multi method new() { nqp::box_n(0e0, self) }
    multi method new($n) { nqp::box_n($n.Num, self) }

    multi method perl(Num:D:) {
        my str $res = self.Str;
        nqp::isnanorinf(nqp::unbox_n(self))
          || nqp::isge_i(nqp::index($res,'e'),0)
          || nqp::isge_i(nqp::index($res,'E'),0)
          ?? $res
          !! nqp::concat($res,'e0')
    }

    method Rat(Num:D: Real $epsilon = 1.0e-6, :$fat) {
        return Rational[Num,Int].new(self,0)
          if nqp::isnanorinf(nqp::unbox_n(self));

        my Num $num = self;
        $num = -$num if (my int $signum = $num < 0);
        my num $r = $num - floor($num);

        # basically have an Int
        if nqp::iseq_n($r,0e0) {
            $fat
              ?? FatRat.new(nqp::fromnum_I(self,Int),1)
              !!    Rat.new(nqp::fromnum_I(self,Int),1)
        }

        # find convergents of the continued fraction.
        else {
            my Int $q = nqp::fromnum_I($num, Int);
            my Int $a = 1;
            my Int $b = $q;
            my Int $c = 0;
            my Int $d = 1;

            while nqp::isne_n($r,0e0) && abs($num - ($b / $d)) > $epsilon {
                my num $modf_arg = 1e0 / $r;
                $q = nqp::fromnum_I($modf_arg, Int);
                $r = $modf_arg - floor($modf_arg);

                my $orig_b = $b;
                $b = $q * $b + $a;
                $a = $orig_b;

                my $orig_d = $d;
                $d = $q * $d + $c;
                $c = $orig_d;
            }

            # Note that this result has less error than any Rational with a
            # smaller denominator but it is not (necessarily) the Rational
            # with the smallest denominator that has less than $epsilon error.
            # However, to find that Rational would take more processing.
            $fat
              ?? FatRat.new($signum ?? -$b !! $b, $d)
              !!    Rat.new($signum ?? -$b !! $b, $d)
        }
    }
    method FatRat(Num:D: Real $epsilon = 1.0e-6) {
        self.Rat($epsilon, :fat);
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
        nqp::p6box_n(nqp::sec_n(nqp::unbox_n(self)));
    }
    proto method asec(|) {*}
    multi method asec(Num:D: ) {
        nqp::p6box_n(nqp::asec_n(nqp::unbox_n(self)));
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
            !! (self + (self * self + 1e0).sqrt).log;
    }
    proto method cosh(|) {*}
    multi method cosh(Num:D: ) {
        nqp::p6box_n(nqp::cosh_n(nqp::unbox_n(self)));
    }
    proto method acosh(|) {*}
    multi method acosh(Num:D: ) {
        (self + (self * self - 1e0).sqrt).log;
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
        nqp::p6box_n(nqp::sech_n(nqp::unbox_n(self)));
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
#?if moar
my constant 𝑒 := e;
#?endif

multi sub prefix:<++>(Num:D $a is rw) {
    $a = nqp::p6box_n(nqp::add_n(nqp::unbox_n($a), 1e0))
}
multi sub prefix:<++>(Num:U $a is rw) {
    $a = 1e0;
}
multi sub prefix:<++>(num $a is rw) returns num {
    $a = nqp::add_n($a, 1e0)
}
multi sub prefix:<-->(Num:D $a is rw) {
    $a = nqp::p6box_n(nqp::sub_n(nqp::unbox_n($a), 1e0))
}
multi sub prefix:<-->(Num:U $a is rw) {
    $a = -1e0;
}
multi sub prefix:<-->(num $a is rw) returns num {
    $a = nqp::sub_n($a, 1e0)
}
multi sub postfix:<++>(Num:D $a is rw) {
    my $b = $a;
    $a = nqp::p6box_n(nqp::add_n(nqp::unbox_n($a), 1e0));
    $b
}
multi sub postfix:<++>(Num:U $a is rw) {
    $a = 1e0;
    0e0
}
multi sub postfix:<++>(num $a is rw) returns num {
    my num $b = $a;
    $a = nqp::add_n($a, 1e0);
    $b
}
multi sub postfix:<-->(Num:D $a is rw) {
    my $b = $a;
    $a = nqp::p6box_n(nqp::sub_n(nqp::unbox_n($a), 1e0));
    $b
}
multi sub postfix:<-->(Num:U $a is rw) {
    $a = -1e0;
    0e0
}
multi sub postfix:<-->(num $a is rw) returns num {
    my num $b = $a;
    $a = nqp::sub_n($a, 1e0);
    $b
}

multi sub prefix:<->(Num:D \a) {
    nqp::p6box_n(nqp::neg_n(nqp::unbox_n(a)))
}
multi sub prefix:<->(num $a) returns num {
    nqp::neg_n($a);
}

multi sub abs(Num:D \a) {
    nqp::p6box_n(nqp::abs_n(nqp::unbox_n(a)))
}
multi sub abs(num $a) returns num {
    nqp::abs_n($a)
}

multi sub infix:<+>(Num:D \a, Num:D \b) {
    nqp::p6box_n(nqp::add_n(nqp::unbox_n(a), nqp::unbox_n(b)))
}
multi sub infix:<+>(num $a, num $b) returns num {
    nqp::add_n($a, $b)
}

multi sub infix:<->(Num:D \a, Num:D \b) {
    nqp::p6box_n(nqp::sub_n(nqp::unbox_n(a), nqp::unbox_n(b)))
}
multi sub infix:<->(num $a, num $b) returns num {
    nqp::sub_n($a, $b)
}

multi sub infix:<*>(Num:D \a, Num:D \b) {
    nqp::p6box_n(nqp::mul_n(nqp::unbox_n(a), nqp::unbox_n(b)))
}
multi sub infix:<*>(num $a, num $b) returns num {
    nqp::mul_n($a, $b)
}

multi sub infix:</>(Num:D \a, Num:D \b) {
    b
      ?? nqp::p6box_n(nqp::div_n(nqp::unbox_n(a), nqp::unbox_n(b)))
      !! Failure.new(X::Numeric::DivideByZero.new(:using</>, :numerator(a)))
}
multi sub infix:</>(num $a, num $b) returns num {
    $b
      ?? nqp::div_n($a, $b)
      !! Failure.new(X::Numeric::DivideByZero.new(:using</>, :numerator($a)))
}

multi sub infix:<%>(Num:D \a, Num:D \b) {
    b
      ?? nqp::p6box_n(nqp::mod_n(nqp::unbox_n(a), nqp::unbox_n(b)))
      !! Failure.new(X::Numeric::DivideByZero.new(:using<%>, :numerator(a)))
}
multi sub infix:<%>(num $a, num $b) returns num {
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
multi sub infix:<**>(num $a, num $b) returns num {
    nqp::pow_n($a, $b)
      or $a == 0e0 || $b.abs == Inf
        ?? 0e0
        !! Failure.new(X::Numeric::Underflow.new)
}

# Here we sort NaN in with string "NaN"
multi sub infix:<cmp>(Num:D \a, Num:D \b) {
     ORDER(nqp::cmp_n(nqp::unbox_n(a), nqp::unbox_n(b))) or
         a === b ?? Same !! a.Stringy cmp b.Stringy;
}
multi sub infix:<cmp>(num $a, num $b) {
    ORDER(nqp::cmp_n($a, $b)) or
         $a === $b ?? Same !! $a.Stringy cmp $b.Stringy;
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
    nqp::p6bool(
      nqp::eqaddr(a.WHAT,b.WHAT)
      && nqp::iseq_n(nqp::unbox_n(a), nqp::unbox_n(b))
    )
}
multi sub infix:<===>(NaN, NaN) {
    True;
}
multi sub infix:<===>(num $a, num $b) returns Bool:D {
    nqp::p6bool(nqp::iseq_n($a, $b))
}

multi sub infix:<==>(Num:D \a, Num:D \b) returns Bool:D  {
    nqp::p6bool(nqp::iseq_n(nqp::unbox_n(a), nqp::unbox_n(b)))
}
multi sub infix:<==>(num $a, num $b) returns Bool:D  {
    nqp::p6bool(nqp::iseq_n($a, $b))
}

multi sub infix:<!=>(num $a, num $b) returns Bool:D {
    nqp::p6bool(nqp::isne_n($a, $b))
}

multi sub infix:«<»(Num:D \a, Num:D \b) returns Bool:D {
    nqp::p6bool(nqp::islt_n(nqp::unbox_n(a), nqp::unbox_n(b)))
}
multi sub infix:«<»(num $a, num $b) returns Bool:D {
    nqp::p6bool(nqp::islt_n($a, $b))
}

multi sub infix:«<=»(Num:D \a, Num:D \b) returns Bool:D {
    nqp::p6bool(nqp::isle_n(nqp::unbox_n(a), nqp::unbox_n(b)))
}
multi sub infix:«<=»(num $a, num $b) returns Bool:D {
    nqp::p6bool(nqp::isle_n($a, $b))
}

multi sub infix:«>»(Num:D \a, Num:D \b) returns Bool:D {
    nqp::p6bool(nqp::isgt_n(nqp::unbox_n(a), nqp::unbox_n(b)))
}
multi sub infix:«>»(num $a, num $b) returns Bool:D {
    nqp::p6bool(nqp::isgt_n($a, $b))
}

multi sub infix:«>=»(Num:D \a, Num:D \b) returns Bool:D {
    nqp::p6bool(nqp::isge_n(nqp::unbox_n(a), nqp::unbox_n(b)))
}
multi sub infix:«>=»(num $a, num $b) returns Bool:D {
    nqp::p6bool(nqp::isge_n($a, $b))
}

sub rand() returns Num:D {
    nqp::p6box_n(nqp::rand_n(1e0));
}

# TODO: default seed of 'time'
sub srand(Int $seed) returns Int:D {
    nqp::p6box_i(nqp::srand($seed))
}

multi sub atan2(Num:D $a, Num:D $b = 1e0) {
    nqp::p6box_n(nqp::atan2_n(nqp::unbox_n($a), nqp::unbox_n($b)));
}

multi sub cosec(Num:D \x) {
    nqp::p6box_n(nqp::div_n(1e0, nqp::sin_n(nqp::unbox_n(x))));
}
multi sub acosec(Num:D \x) {
    nqp::p6box_n(nqp::asin_n(nqp::div_n(1e0, nqp::unbox_n(x))));
}

multi sub log(num $x) returns num {
    nqp::log_n($x);
}

multi sub sin(num $x) returns num {
    nqp::sin_n($x);
}
multi sub asin(num $x) returns num {
    nqp::asin_n($x);
}
multi sub cos(num $x) returns num {
    nqp::cos_n($x);
}
multi sub acos(num $x) returns num {
    nqp::acos_n($x);
}
multi sub tan(num $x) returns num {
    nqp::tan_n($x);
}
multi sub atan(num $x) returns num {
    nqp::atan_n($x);
}
multi sub sec(num $x) returns num {
    nqp::sec_n($x);
}
multi sub asec(num $x) returns num {
    nqp::asec_n($x);
}

multi sub cotan(num $x) returns num {
    nqp::div_n(1e0, nqp::tan_n($x));
}
multi sub acotan(num $x) returns num {
    nqp::atan_n(nqp::div_n(1e0, $x));
}
multi sub sinh(num $x) returns num {
    nqp::sinh_n($x);
}
multi sub asinh(num $x) returns num {
    # ln(x + √(x²+1))
    nqp::isnanorinf($x)
        ?? $x
        !! nqp::log_n(
            nqp::add_n(
                $x,
                nqp::pow_n( nqp::add_n(nqp::mul_n($x,$x), 1e0), .5e0 )
            )
        )
}

multi sub cosh(num $x) returns num {
    nqp::cosh_n($x);
}
multi sub acosh(num $x) returns num {
    # ln(x + √(x²-1))
    nqp::log_n(
        nqp::add_n(
            $x,
            nqp::pow_n( nqp::sub_n(nqp::mul_n($x,$x), 1e0), .5e0 )
        )
    )
}
multi sub tanh(num $x) returns num {
    nqp::tanh_n($x);
}
multi sub atanh(num $x) returns num {
    log((1e0 + $x) / (1e0 - $x)) / 2e0;
}
multi sub sech(num $x) returns num {
    nqp::sech_n($x);
}
multi sub asech(num $x) returns num {
    acosh(1e0 / $x);
}
multi sub cosech(num $x) returns num {
    1e0 / sinh($x)
}
multi sub acosech(num $x) returns num {
    asinh(1e0 / $x);
}
multi sub cotanh(num $x) returns num {
    1e0 / tanh($x);
}
multi sub acotanh(num $x) returns num {
    atanh(1e0 / $x)
}

multi sub floor(num $a) returns num {
    nqp::floor_n($a)
}
multi sub ceiling(num $a) returns num {
    nqp::ceil_n($a)
}
multi sub sqrt(num $a) returns num {
    nqp::sqrt_n($a)
}

# vim: ft=perl6 expandtab sw=4
