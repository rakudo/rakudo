
# XXX: Temporary definition of $Inf and $NaN until we have constants available
# constant Inf = ...
# constant NaN = ...
my $Inf = nqp::p6box_n(pir::set__Ns('Inf'));
my $NaN = nqp::p6box_n(pir::set__Ns('NaN'));

my class Num {
    method Num() { self }
    method Bridge(Num:D:) { self }
    
    method Int(Num:D:) {
        nqp::p6box_i(nqp::unbox_n(self));
    }

    multi method new() { nqp::p6box_n(0) }
    multi method new($n as Num) { $n }
    
    multi method perl(Num:D:) {
        my $res = self.Str;
        if $res.index('e').defined {
            $res;
        } else {
            $res ~ 'e0';
        }
    }

    method Rat(Num:D: Real $epsilon = 1.0e-6) {
        my sub modf($num) { my $q = $num.Int; $num - $q, $q; }

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

        ($signum * $b) / $d;
    }

    multi method atan2(Num:D: Num:D $x = 1e0) {
        nqp::p6box_n(pir::atan__NNn(nqp::unbox_n(self), nqp::unbox_n($x)));
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
        nqp::p6box_n(pir::exp__Nn(nqp::unbox_n(self)));
    }

    proto method log(|$) {*}
    multi method log(Num:D: ) {
        nqp::p6box_n(pir::ln__NN(nqp::unbox_n(self)));
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
        nqp::p6box_n(pir::ceil__NN(nqp::unbox_n(self)));
    }
    method floor(Num:D: ) {
        nqp::p6bigint(pir::floor__NN(nqp::unbox_n(self)));
    }

    proto method sin(|$) {*}
    multi method sin(Num:D: ) {
        nqp::p6box_n(pir::sin__NN(nqp::unbox_n(self)));
    }
    proto method asin(|$) {*}
    multi method asin(Num:D: ) {
        nqp::p6box_n(pir::asin__NN(nqp::unbox_n(self)));
    }
    proto method cos(|$) {*}
    multi method cos(Num:D: ) {
        nqp::p6box_n(pir::cos__NN(nqp::unbox_n(self)));
    }
    proto method acos(|$) {*}
    multi method acos(Num:D: ) {
        nqp::p6box_n(pir::acos__NN(nqp::unbox_n(self)));
    }
    proto method tan(|$) {*}
    multi method tan(Num:D: ) {
        nqp::p6box_n(pir::tan__NN(nqp::unbox_n(self)));
    }
    proto method atan(|$) {*}
    multi method atan(Num:D: ) {
        nqp::p6box_n(pir::atan__NN(nqp::unbox_n(self)));
    }
    proto method sec(|$) {*}
    multi method sec(Num:D: ) {
        nqp::p6box_n(pir::sec__NN(nqp::unbox_n(self)));
    }
    proto method asec(|$) {*}
    multi method asec(Num:D: ) {
        nqp::p6box_n(pir::asec__NN(nqp::unbox_n(self)));
    }
    method cosec(Num:D:) {
        nqp::p6box_n(nqp::div_n(1, pir::sin__NN(nqp::unbox_n(self))));
    }
    method acosec(Num:D:) {
        nqp::p6box_n(pir::asin__NN(nqp::div_n(1, nqp::unbox_n(self))));
    }
    method cotan(Num:D:) {
        nqp::p6box_n(nqp::div_n(1, pir::tan__NN(nqp::unbox_n(self))));
    }
    method acotan(Num:D:) {
        nqp::p6box_n(pir::atan__NN(nqp::div_n(1, nqp::unbox_n(self))));
    }
    proto method sinh(|$) {*}
    multi method sinh(Num:D: ) {
        nqp::p6box_n(pir::sinh__NN(nqp::unbox_n(self)));
    }
    proto method asinh(|$) {*}
    multi method asinh(Num:D: ) {
        (self + (self * self + 1).sqrt).log;
    }
    proto method cosh(|$) {*}
    multi method cosh(Num:D: ) {
        nqp::p6box_n(pir::cosh__NN(nqp::unbox_n(self)));
    }
    proto method acosh(|$) {*}
    multi method acosh(Num:D: ) {
        (self + (self * self - 1).sqrt).log;
    }
    proto method tanh(|$) {*}
    multi method tanh(Num:D: ) {
        nqp::p6box_n(pir::tanh__NN(nqp::unbox_n(self)));
    }
    proto method atanh(|$) {*}
    multi method atanh(Num:D: ) {
        ((1 + self) / (1 - self)).log / 2;
    }
    proto method sech(|$) {*}
    multi method sech(Num:D: ) {
        nqp::p6box_n(pir::sech__NN(nqp::unbox_n(self)));
    }
    proto method asech(|$) {*}
    multi method asech(Num:D: ) {
        (1 / self).acosh;
    }
    proto method cosech(|$) {*}
    multi method cosech(Num:D: ) {
        nqp::p6box_n(nqp::div_n(1, pir::sinh__NN(nqp::unbox_n(self))));
    }
    proto method acosech(|$) {*}
    multi method acosech(Num:D: ) {
        (1 / self).asinh;
    }
    proto method cotanh(|$) {*}
    multi method cotanh(Num:D: ) {
        nqp::p6box_n(nqp::div_n(1, pir::tanh__NN(nqp::unbox_n(self))));
    }
    proto method acotanh(|$) {*}
    multi method acotanh(Num:D: ) {
        (1 / self).atanh;
    }
}

my constant pi = 3.14159265e0;
my constant e  = 2.71828183e0;

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
    nqp::want(
        nqp::p6box_n(nqp::neg_n($a)),
        'Nn', nqp::neg_n($a),
    );
}

multi prefix:<abs>(Num:D \$a) {
    nqp::p6box_n(nqp::abs_n(nqp::unbox_n($a)))
}
multi prefix:<abs>(num $a) {
    nqp::want(
        nqp::p6box_n(nqp::abs_n($a)),
        'Nn', nqp::abs_n($a)
    );
}

multi infix:<+>(Num:D \$a, Num:D \$b) {
    nqp::p6box_n(nqp::add_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}
multi infix:<+>(num $a, num $b) {
    nqp::want(
        nqp::p6box_n(nqp::add_n($a, $b)),
        'Nn', nqp::add_n($a, $b)
    );
}

multi infix:<->(Num:D \$a, Num:D \$b) {
    nqp::p6box_n(nqp::sub_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}
multi infix:<->(num $a, num $b) {
    nqp::want(
        nqp::p6box_n(nqp::sub_n($a, $b)),
        'Nn', nqp::sub_n($a, $b)
    );
}

multi infix:<*>(Num:D \$a, Num:D \$b) {
    nqp::p6box_n(nqp::mul_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}
multi infix:<*>(num \$a, num \$b) {
    nqp::want(
        nqp::p6box_n(nqp::mul_n($a, $b)),
        'Nn', nqp::mul_n($a, $b)
    );
}

multi infix:</>(Num:D \$a, Num:D \$b) {
    nqp::p6box_n(nqp::div_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}
multi infix:</>(num $a, num $b) {
    nqp::want(
        nqp::p6box_n(nqp::div_n($a, $b)),
        'Nn', nqp::div_n($a, $b)
    );
}

multi infix:<%>(Num:D \$a, Num:D \$b) {
    nqp::p6box_n(nqp::mod_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}
multi infix:<%>(num $a, num $b) {
    nqp::want(
        nqp::p6box_n(nqp::mod_n($a, $b)),
        'Nn', nqp::mod_n($a, $b)
    );
}

multi infix:<**>(Num:D \$a, Num:D \$b) {
    nqp::p6box_n(nqp::pow_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}
multi infix:<**>(num $a, num $b) {
    nqp::want(
        nqp::p6box_n(nqp::pow_n($a, $b)),
        'Nn', nqp::pow_n($a, $b)
    );
}


multi infix:<cmp>(Num:D \$a, Num:D \$b) {
    nqp::p6box_i(nqp::cmp_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}
multi infix:<cmp>(num $a, num $b) {
    nqp::want(
        nqp::p6box_i(nqp::cmp_n($a, $b)),
        'Ii', nqp::cmp_n($a, $b)
    );
}

multi infix:«<=>»(Num:D \$a, Num:D \$b) {
    nqp::p6box_i(nqp::cmp_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}
multi infix:«<=>»(num $a, num $b) {
    nqp::want(
        nqp::p6box_i(nqp::cmp_n($a, $b)),
        'Ii', nqp::cmp_n($a, $b)
    );
}

multi infix:<===>(Num:D \$a, Num:D \$b) {
    nqp::p6bool(nqp::iseq_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}
multi infix:<===>(num $a, num $b) {
    nqp::p6bool(nqp::iseq_n($a, $b))
}

multi infix:<==>(Num:D \$a, Num:D \$b) {
    nqp::p6bool(nqp::iseq_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}
multi infix:<==>(num $a, num $b) {
    nqp::p6bool(nqp::iseq_n($a, $b))
}

multi infix:<!=>(Num:D \$a, Num:D \$b) {
    nqp::p6bool(nqp::isne_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}
multi infix:<!=>(num $a, num $b) {
    nqp::p6bool(nqp::isne_n($a, $b))
}

multi infix:«<»(Num:D \$a, Num:D \$b) {
    nqp::p6bool(nqp::islt_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}
multi infix:«<»(num $a, num $b) {
    nqp::p6bool(nqp::islt_n($a, $b))
}

multi infix:«<=»(Num:D \$a, Num:D \$b) {
    nqp::p6bool(nqp::isle_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}
multi infix:«<=»(num $a, num $b) {
    nqp::p6bool(nqp::isle_n($a, $b))
}

multi infix:«>»(Num:D \$a, Num:D \$b) {
    nqp::p6bool(nqp::isgt_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}
multi infix:«>»(num $a, num $b) {
    nqp::p6bool(nqp::isgt_n($a, $b))
}

multi infix:«>=»(Num:D \$a, Num:D \$b) {
    nqp::p6bool(nqp::isge_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}
multi infix:«>=»(num $a, num $b) {
    nqp::p6bool(nqp::isge_n($a, $b))
}

sub rand() {
    nqp::want(
        nqp::p6box_n(pir::rand__NN(1)),
        'Nn', pir::rand__NN(1),
    );
}

# TODO: default seed of 'time'
sub srand(Int $seed) {
    nqp::p6box_i(pir::srand__0I($seed))
}

multi sub atan2(Num:D $a, Num:D $b = 1e0) {
    nqp::p6box_n(pir::atan__NNn(nqp::unbox_n($a), nqp::unbox_n($b)));
}

multi sub cosec(Num:D \$x) {
    nqp::p6box_n(nqp::div_n(1, pir::sin__NN(nqp::unbox_n($x))));
}
multi sub acosec(Num:D \$x) {
    nqp::p6box_n(pir::asin__NN(nqp::div_n(1, nqp::unbox_n($x))));
}

multi sub log(num $x) {
    nqp::want(nqp::p6box_n(pir::ln__NN($x)),
            'Nn', pir::ln__NN($x));
}

multi sub sin(num $x) {
    nqp::want(nqp::p6box_n(pir::sin__NN($x)),
            'Nn', pir::sin__NN($x));
}
multi sub asin(num $x) {
    nqp::want(nqp::p6box_n(pir::asin__NN($x)),
            'Nn', pir::asin__NN($x));
}
multi sub cos(num $x) {
    nqp::want(nqp::p6box_n(pir::cos__NN($x)),
            'Nn', pir::cos__NN($x));
}
multi sub acos(num $x) {
    nqp::want(nqp::p6box_n(pir::acos__NN($x)),
            'Nn', pir::acos__NN($x));
}
multi sub tan(num $x) {
    nqp::want(nqp::p6box_n(pir::tan__NN($x)),
            'Nn', pir::tan__NN($x));
}
multi sub atan(num $x) {
    nqp::want(nqp::p6box_n(pir::atan__NN($x)),
            'Nn', pir::atan__NN($x));
}
multi sub sec(num $x) {
    nqp::want(nqp::p6box_n(pir::sec__NN($x)),
            'Nn', pir::sec__NN($x));
}
multi sub asec(num $x) {
    nqp::want(nqp::p6box_n(pir::asec__NN($x)),
            'Nn', pir::asec__NN($x));
}

multi sub cotan(num $x) {
    nqp::want(nqp::p6box_n(nqp::div_n(1, pir::tan__NN($x))),
        'Nn', nqp::div_n(1, pir::tan__NN($x)));
}
multi sub acotan(num $x) {
    nqp::want(nqp::p6box_n(nqp::div_n(1, pir::tan__NN($x))),
        'Nn', nqp::div_n(1, pir::atan__NN($x)));
}
multi sub sinh(num $x) {
    nqp::want(nqp::p6box_n(pir::sinh__NN($x)),
            'Nn', pir::sinh__NN($x));
}
multi sub asinh(num $x) {
    log($x + ($x * $x + 1e0));
}

multi sub cosh(num $x) {
    nqp::want(nqp::p6box_n(pir::cosh__NN($x)),
            'Nn', pir::cosh__NN($x));
}
multi sub acosh(num $x) {
    log($x + ($x * $x - 1e0))
}
multi sub tanh(num $x) {
    nqp::want(nqp::p6box_n(pir::tanh__NN($x)),
            'Nn', pir::tanh__NN($x));
}
multi sub atanh(num $x) {
    log((1 + $x) / (1 - $x)) / 2e0;
}
multi sub sech(num $x) {
    nqp::want(nqp::p6box_n(pir::sech__NN($x)),
            'Nn', pir::sech__NN($x));
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
