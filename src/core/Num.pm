
# XXX: Temporary definition of $Inf and $NaN until we have constants available
# constant Inf = ...
# constant NaN = ...
my $Inf = nqp::p6box_n(pir::set__Ns('Inf'));
my $NaN = nqp::p6box_n(pir::set__Ns('NaN'));

my class Num {
    method Num() { self }
    method Bridge() { self }
    
    method Int() {
        nqp::p6box_i(nqp::unbox_n(self));
    }

    method Rat(Real $epsilon = 1.0e-6) {
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

    method atan2(Num $x = 1e0) {
        nqp::p6box_n(pir::atan__NNn(nqp::unbox_n(self), nqp::unbox_n($x)));
    }

    multi method Str(Num:D:) {
        nqp::p6box_s(nqp::unbox_n(self));
    }

    method succ(Num:D:) { self + 1e0 }

    method pred(Num:D:) { self - 1e0 }

    method isNaN() {
        self != self;
    }

    method abs() {
        nqp::p6box_n(nqp::abs_n(nqp::unbox_n(self)));
    }

    method exp() {
        nqp::p6box_n(pir::exp__Nn(nqp::unbox_n(self)));
    }

    proto method log(|$) {*}
    multi method log() {
        nqp::p6box_n(pir::ln__NN(nqp::unbox_n(self)));
    }
    multi method log(Num \$base) {
        self.log() / $base.log();
    }

    proto method sqrt(|$) {*}
    multi method sqrt() {
        nqp::p6box_n(pir::sqrt__NN(nqp::unbox_n(self)));
    }

    method rand() {
        nqp::p6box_n(pir::rand__NN(nqp::unbox_n(self)));
    }

    method ceil() {
        nqp::p6box_n(pir::ceil__NN(nqp::unbox_n(self)));
    }
    method floor() {
        nqp::p6box_n(pir::floor__NN(nqp::unbox_n(self)));
    }

    proto method sin(|$) {*}
    multi method sin() {
        nqp::p6box_n(pir::sin__NN(nqp::unbox_n(self)));
    }
    proto method asin(|$) {*}
    multi method asin() {
        nqp::p6box_n(pir::asin__NN(nqp::unbox_n(self)));
    }
    proto method cos(|$) {*}
    multi method cos() {
        nqp::p6box_n(pir::cos__NN(nqp::unbox_n(self)));
    }
    proto method acos(|$) {*}
    multi method acos() {
        nqp::p6box_n(pir::acos__NN(nqp::unbox_n(self)));
    }
    proto method tan(|$) {*}
    multi method tan() {
        nqp::p6box_n(pir::tan__NN(nqp::unbox_n(self)));
    }
    proto method atan(|$) {*}
    multi method atan() {
        nqp::p6box_n(pir::atan__NN(nqp::unbox_n(self)));
    }
    proto method sec(|$) {*}
    multi method sec() {
        nqp::p6box_n(pir::sec__NN(nqp::unbox_n(self)));
    }
    proto method asec(|$) {*}
    multi method asec() {
        nqp::p6box_n(pir::asec__NN(nqp::unbox_n(self)));
    }
    proto method sinh(|$) {*}
    multi method sinh() {
        nqp::p6box_n(pir::sinh__NN(nqp::unbox_n(self)));
    }
    proto method cosh(|$) {*}
    multi method cosh() {
        nqp::p6box_n(pir::cosh__NN(nqp::unbox_n(self)));
    }
    proto method tanh(|$) {*}
    multi method tanh() {
        nqp::p6box_n(pir::tanh__NN(nqp::unbox_n(self)));
    }
    proto method sech(|$) {*}
    multi method sech() {
        nqp::p6box_n(pir::sech__NN(nqp::unbox_n(self)));
    }
}

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

multi prefix:<->(Num \$a) {
    nqp::p6box_n(nqp::neg_n(nqp::unbox_n($a)))
}

multi prefix:<abs>(Num \$a) {
    nqp::p6box_n(nqp::abs_n(nqp::unbox_n($a)))
}

multi infix:<+>(Num \$a, Num \$b) {
    nqp::p6box_n(nqp::add_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}

multi infix:<->(Num \$a, Num \$b) {
    nqp::p6box_n(nqp::sub_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}

multi infix:<*>(Num \$a, Num \$b) {
    nqp::p6box_n(nqp::mul_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}

multi infix:</>(Num \$a, Num \$b) {
    nqp::p6box_n(nqp::div_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}

multi infix:<%>(Num \$a, Num \$b) {
    nqp::p6box_n(nqp::mod_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}

multi infix:<**>(Num \$a, Num \$b) {
    nqp::p6box_n(nqp::pow_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}


multi infix:<cmp>(Num \$a, Num \$b) {
    nqp::p6box_n(nqp::cmp_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}

multi infix:«<=>»(Num \$a, Num \$b) {
    nqp::p6box_n(nqp::cmp_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}

multi infix:<===>(Num \$a, Num \$b) {
    nqp::p6bool(nqp::iseq_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}

multi infix:<==>(Num \$a, Num \$b) {
    nqp::p6bool(nqp::iseq_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}

multi infix:<!=>(Num \$a, Num \$b) {
    nqp::p6bool(nqp::isne_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}

multi infix:«<»(Num \$a, Num \$b) {
    nqp::p6bool(nqp::islt_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}

multi infix:«<=»(Num \$a, Num \$b) {
    nqp::p6bool(nqp::isle_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}

multi infix:«>»(Num \$a, Num \$b) {
    nqp::p6bool(nqp::isgt_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}

multi infix:«>=»(Num \$a, Num \$b) {
    nqp::p6bool(nqp::isge_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}

