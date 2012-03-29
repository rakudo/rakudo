
my class Num {
    multi method WHICH(Num:D:) {
        nqp::box_s(
            nqp::concat_s(
                nqp::concat_s(nqp::unbox_s(self.^name), '|'),
                nqp::unbox_n(self)
            ),
            ObjAt
        );
    }
    method Num() { self }
    method Bridge(Num:D:) { self }
    
    method Int(Num:D:) {
        (self == $Inf || self == -$Inf) ??
            fail("Cannot coerce Inf to an Int") !!
            nqp::fromnum_I(nqp::unbox_n(self), Int);
    }

    multi method new() { nqp::p6box_n(0) }
    multi method new($n as Num) { $n }
    
    multi method perl(Num:D:) {
        my $res = self.Str;
        if nqp::isnanorinf(nqp::unbox_n(self)) || $res.index('e').defined {
            $res;
        } else {
            $res ~ 'e0';
        }
    }

    method Rat(Num:D: Real $epsilon = 1.0e-6, :$fat) {
        my sub modf($num) { my $q = $num.Int; $num - $q, $q; }

        (self == $Inf || self == -$Inf) && fail("Cannot coerce Inf to a Rat");
        
        my Num $num = self;
        my Int $signum = $num < 0 ?? -1 !! 1;
        $num = -$num if $signum == -1;

        # Find convergents of the continued fraction.

        my Num $r = $num - $num.Int;
        my Int $q = $num.Int;
        my ($a, $b) = 1, $q;
        my ($c, $d) = 0, 1;

        while $r != 0 && abs($num - ($b/$d)) > $epsilon {
            ($r, $q) = modf(1/$r);

            ($a, $b) = ($b, $q*$b + $a);
            ($c, $d) = ($d, $q*$d + $c);
        }

        # Note that this result has less error than any Rational with a
        # smaller denominator but it is not (necessarily) the Rational
        # with the smallest denominator that has less than $epsilon error.
        # However, to find that Rational would take more processing.
        $fat ?? FatRat.new($signum * $b, $d) !! ($signum * $b) / $d;
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

    proto method log(|$) {*}
    multi method log(Num:D: ) {
        nqp::p6box_n(nqp::log_n(nqp::unbox_n(self)));
    }
    multi method log(Num:D: Num \$base) {
        self.log() / $base.log();
    }

    proto method sqrt(|$) {*}
    multi method sqrt(Num:D: ) {
        nqp::p6box_n(nqp::sqrt_n(nqp::unbox_n(self)));
    }

    method rand(Num:D: ) {
        nqp::p6box_n(pir::rand__NN(nqp::unbox_n(self)));
    }

    method ceiling(Num:D: ) {
        nqp::isnanorinf(nqp::unbox_n(self))
            ?? self
            !! nqp::fromnum_I(pir::ceil__NN(nqp::unbox_n(self)), Int);
    }
    method floor(Num:D: ) {
        nqp::isnanorinf(nqp::unbox_n(self))
            ?? self
            !! nqp::fromnum_I(pir::floor__NN(nqp::unbox_n(self)), Int);
    }

    proto method sin(|$) {*}
    multi method sin(Num:D: ) {
        nqp::p6box_n(nqp::sin_n(nqp::unbox_n(self)));
    }
    proto method asin(|$) {*}
    multi method asin(Num:D: ) {
        nqp::p6box_n(nqp::asin_n(nqp::unbox_n(self)));
    }
    proto method cos(|$) {*}
    multi method cos(Num:D: ) {
        nqp::p6box_n(nqp::cos_n(nqp::unbox_n(self)));
    }
    proto method acos(|$) {*}
    multi method acos(Num:D: ) {
        nqp::p6box_n(nqp::acos_n(nqp::unbox_n(self)));
    }
    proto method tan(|$) {*}
    multi method tan(Num:D: ) {
        nqp::p6box_n(nqp::tan_n(nqp::unbox_n(self)));
    }
    proto method atan(|$) {*}
    multi method atan(Num:D: ) {
        nqp::p6box_n(nqp::atan_n(nqp::unbox_n(self)));
    }
    proto method sec(|$) {*}
    multi method sec(Num:D: ) {
        nqp::p6box_n(nqp::sec_n(nqp::unbox_n(self)));
    }
    proto method asec(|$) {*}
    multi method asec(Num:D: ) {
        nqp::p6box_n(nqp::asec_n(nqp::unbox_n(self)));
    }
    method cosec(Num:D:) {
        nqp::p6box_n(nqp::div_n(1, nqp::sin_n(nqp::unbox_n(self))));
    }
    method acosec(Num:D:) {
        nqp::p6box_n(nqp::asin_n(nqp::div_n(1, nqp::unbox_n(self))));
    }
    method cotan(Num:D:) {
        nqp::p6box_n(nqp::div_n(1, nqp::tan_n(nqp::unbox_n(self))));
    }
    method acotan(Num:D:) {
        nqp::p6box_n(nqp::atan_n(nqp::div_n(1, nqp::unbox_n(self))));
    }
    proto method sinh(|$) {*}
    multi method sinh(Num:D: ) {
        nqp::p6box_n(nqp::sinh_n(nqp::unbox_n(self)));
    }
    proto method asinh(|$) {*}
    multi method asinh(Num:D: ) {
        (self + (self * self + 1).sqrt).log;
    }
    proto method cosh(|$) {*}
    multi method cosh(Num:D: ) {
        nqp::p6box_n(nqp::cosh_n(nqp::unbox_n(self)));
    }
    proto method acosh(|$) {*}
    multi method acosh(Num:D: ) {
        (self + (self * self - 1).sqrt).log;
    }
    proto method tanh(|$) {*}
    multi method tanh(Num:D: ) {
        nqp::p6box_n(nqp::tanh_n(nqp::unbox_n(self)));
    }
    proto method atanh(|$) {*}
    multi method atanh(Num:D: ) {
        ((1 + self) / (1 - self)).log / 2;
    }
    proto method sech(|$) {*}
    multi method sech(Num:D: ) {
        nqp::p6box_n(nqp::sech_n(nqp::unbox_n(self)));
    }
    proto method asech(|$) {*}
    multi method asech(Num:D: ) {
        (1 / self).acosh;
    }
    proto method cosech(|$) {*}
    multi method cosech(Num:D: ) {
        nqp::p6box_n(nqp::div_n(1, nqp::sinh_n(nqp::unbox_n(self))));
    }
    proto method acosech(|$) {*}
    multi method acosech(Num:D: ) {
        (1 / self).asinh;
    }
    proto method cotanh(|$) {*}
    multi method cotanh(Num:D: ) {
        nqp::p6box_n(nqp::div_n(1, nqp::tanh_n(nqp::unbox_n(self))));
    }
    proto method acotanh(|$) {*}
    multi method acotanh(Num:D: ) {
        (1 / self).atanh;
    }
}

my constant pi = 3.14159_26535_89793_238e0;
my constant e  = 2.71828_18284_59045_235e0;

multi prefix:<++>(Num:D \$a is rw) {   # XXX
    $a = nqp::p6box_n(nqp::add_n(nqp::unbox_n($a), 1))
}
multi prefix:<++>(Num:U \$a is rw) {   # XXX
    $a = 1e0;
}
multi prefix:<-->(Num:D \$a is rw) {   # XXX
    $a = nqp::p6box_n(nqp::sub_n(nqp::unbox_n($a), 1))
}
multi prefix:<-->(Num:U \$a is rw) {   # XXX
    $a = -1e0;
}
multi postfix:<++>(Num:D \$a is rw) {  # XXX
    my $b = $a;
    $a = nqp::p6box_n(nqp::add_n(nqp::unbox_n($a), 1));
    $b
}
multi postfix:<++>(Num:U \$a is rw) {   # XXX
    $a = 1e0;
    0
}
multi postfix:<-->(Num:D \$a is rw) {  # XXX
    my $b = $a;
    $a = nqp::p6box_n(nqp::sub_n(nqp::unbox_n($a), 1));
    $b
}
multi postfix:<-->(Num:U \$a is rw) {   # XXX
    $a = -1e0;
    0
}

multi prefix:<->(Num:D \$a) {
    nqp::p6box_n(nqp::neg_n(nqp::unbox_n($a)))
}
multi prefix:<->(num $a) {
    nqp::neg_n($a);
}

multi prefix:<abs>(Num:D \$a) {
    nqp::p6box_n(nqp::abs_n(nqp::unbox_n($a)))
}
multi prefix:<abs>(num $a) {
    nqp::abs_n($a)
}

multi infix:<+>(Num:D \$a, Num:D \$b) {
    nqp::p6box_n(nqp::add_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}
multi infix:<+>(num $a, num $b) {
    nqp::add_n($a, $b)
}

multi infix:<->(Num:D \$a, Num:D \$b) {
    nqp::p6box_n(nqp::sub_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}
multi infix:<->(num $a, num $b) {
    nqp::sub_n($a, $b)
}

multi infix:<*>(Num:D \$a, Num:D \$b) {
    nqp::p6box_n(nqp::mul_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}
multi infix:<*>(num \$a, num \$b) {
    nqp::mul_n($a, $b)
}

multi infix:</>(Num:D \$a, Num:D \$b) {
    nqp::p6box_n(nqp::div_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}
multi infix:</>(num $a, num $b) {
    nqp::div_n($a, $b)
}

multi infix:<%>(Num:D \$a, Num:D \$b) {
    nqp::p6box_n(nqp::mod_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}
multi infix:<%>(num $a, num $b) {
    nqp::mod_n($a, $b)
}

multi infix:<**>(Num:D \$a, Num:D \$b) {
    nqp::p6box_n(nqp::pow_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}
multi infix:<**>(num $a, num $b) {
    nqp::pow_n($a, $b)
}


multi infix:<cmp>(Num:D \$a, Num:D \$b) {
    nqp::p6box_i(nqp::cmp_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}
multi infix:<cmp>(num $a, num $b) {
    nqp::cmp_n($a, $b)
}

multi infix:«<=>»(Num:D \$a, Num:D \$b) {
    Order.(nqp::p6box_i(nqp::cmp_n(nqp::unbox_n($a), nqp::unbox_n($b))))
}
multi infix:«<=>»(num $a, num $b) {
    Order.(nqp::cmp_n($a, $b))
}

multi infix:<===>(Num:D \$a, Num:D \$b) {
    nqp::p6bool(nqp::iseq_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}
multi infix:<===>(num $a, num $b) returns Bool:D {
    nqp::p6bool(nqp::iseq_n($a, $b))
}

multi infix:<==>(Num:D \$a, Num:D \$b) returns Bool:D  {
    nqp::p6bool(nqp::iseq_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}
multi infix:<==>(num $a, num $b) returns Bool:D  {
    nqp::p6bool(nqp::iseq_n($a, $b))
}

multi infix:<!=>(num $a, num $b) returns Bool:D {
    nqp::p6bool(nqp::isne_n($a, $b))
}

multi infix:«<»(Num:D \$a, Num:D \$b) returns Bool:D {
    nqp::p6bool(nqp::islt_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}
multi infix:«<»(num $a, num $b) returns Bool:D {
    nqp::p6bool(nqp::islt_n($a, $b))
}

multi infix:«<=»(Num:D \$a, Num:D \$b) returns Bool:D {
    nqp::p6bool(nqp::isle_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}
multi infix:«<=»(num $a, num $b) returns Bool:D {
    nqp::p6bool(nqp::isle_n($a, $b))
}

multi infix:«>»(Num:D \$a, Num:D \$b) returns Bool:D {
    nqp::p6bool(nqp::isgt_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}
multi infix:«>»(num $a, num $b) returns Bool:D {
    nqp::p6bool(nqp::isgt_n($a, $b))
}

multi infix:«>=»(Num:D \$a, Num:D \$b) returns Bool:D {
    nqp::p6bool(nqp::isge_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}
multi infix:«>=»(num $a, num $b) returns Bool:D {
    nqp::p6bool(nqp::isge_n($a, $b))
}

sub rand() returns Num:D {
    nqp::p6box_n(pir::rand__NN(1));
}

# TODO: default seed of 'time'
sub srand(Int $seed) returns Int:D {
    nqp::p6box_i(pir::srand__0I($seed))
}

multi sub atan2(Num:D $a, Num:D $b = 1e0) {
    nqp::p6box_n(nqp::atan2_n(nqp::unbox_n($a), nqp::unbox_n($b)));
}

multi sub cosec(Num:D \$x) {
    nqp::p6box_n(nqp::div_n(1, nqp::sin_n(nqp::unbox_n($x))));
}
multi sub acosec(Num:D \$x) {
    nqp::p6box_n(nqp::asin_n(nqp::div_n(1, nqp::unbox_n($x))));
}

multi sub log(num $x) {
    nqp::log_n($x);
}

multi sub sin(num $x) {
    nqp::sin_n($x);
}
multi sub asin(num $x) {
    nqp::asin_n($x);
}
multi sub cos(num $x) {
    nqp::cos_n($x);
}
multi sub acos(num $x) {
    nqp::acos_n($x);
}
multi sub tan(num $x) {
    nqp::tan_n($x);
}
multi sub atan(num $x) {
    nqp::atan_n($x);
}
multi sub sec(num $x) {
    nqp::sec_n($x);
}
multi sub asec(num $x) {
    nqp::asec_n($x);
}

multi sub cotan(num $x) {
    nqp::div_n(1, nqp::tan_n($x));
}
multi sub acotan(num $x) {
    nqp::div_n(1, nqp::atan_n($x));
}
multi sub sinh(num $x) {
    nqp::sinh_n($x);
}
multi sub asinh(num $x) {
    log($x + ($x * $x + 1e0));
}

multi sub cosh(num $x) {
    nqp::cosh_n($x);
}
multi sub acosh(num $x) {
    log($x + ($x * $x - 1e0))
}
multi sub tanh(num $x) {
    nqp::tanh_n($x);
}
multi sub atanh(num $x) {
    log((1 + $x) / (1 - $x)) / 2e0;
}
multi sub sech(num $x) {
    nqp::sech_n($x);
}
multi sub asech(num $x) {
    acosh(1e0 / $x);
}
multi sub cosech(num $x) {
    1e0 / sinh($x)
}
multi sub acosech(num $x) {
    asinh(1e0 / $x);
}
multi sub cotanh(num $x) {
    1e0 / tanh($x);
}
multi sub acotanh(num $x) {
    atanh(1e0 / $x)
}
