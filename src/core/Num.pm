
# XXX: Temporary definition of $Inf and $NaN until we have constants available
# constant Inf = ...
# constant NaN = ...
my $Inf = pir::perl6_box_num__PN(pir::set__Ns('Inf'));
my $NaN = pir::perl6_box_num__PN(pir::set__Ns('NaN'));

my class Num {
    method Num() { self }
    method Bridge() { self }
    
    method Int() {
        pir::perl6_box_int__PI(pir::repr_unbox_num__NP(self));
    }
    
    multi method Str(Num:D:) {
        pir::perl6_box_str__PS(pir::repr_unbox_num__NP(self));
    }

    proto method succ(|$) {*}
    multi method succ(Num:D:) { self + 1e0 }
    multi method succ(Num:U:) {        1e0 }

    proto method pred(|$) {*}
    multi method pred(Num:D:) { self - 1e0 }
    multi method pred(Num:U:) {       -1e0 }

    method isNaN() {
        self != self;
    }

    method abs() {
        pir::perl6_box_num__PN(pir::abs__NN(pir::repr_unbox_num__NP(self)));
    }

    proto method log(|$) {*}
    multi method log() {
        pir::perl6_box_num__PN(pir::ln__NN(pir::repr_unbox_num__NP(self)));
    }
    multi method log(Num \$base) {
        self.log() / $base.log();
    }

    proto method sqrt(|$) {*}
    multi method sqrt() {
        pir::perl6_box_num__PN(pir::sqrt__NN(pir::repr_unbox_num__NP(self)));
    }

    method rand() {
        pir::perl6_box_num__PN(pir::rand__NN(pir::repr_unbox_num__NP(self)));
    }

    method ceil() {
        pir::perl6_box_num__PN(pir::ceil__NN(pir::repr_unbox_num__NP(self)));
    }
    method floor() {
        pir::perl6_box_num__PN(pir::floor__NN(pir::repr_unbox_num__NP(self)));
    }

    proto method sin(|$) {*}
    multi method sin() {
        pir::perl6_box_num__PN(pir::sin__NN(pir::repr_unbox_num__NP(self)));
    }
    proto method asin(|$) {*}
    multi method asin() {
        pir::perl6_box_num__PN(pir::asin__NN(pir::repr_unbox_num__NP(self)));
    }
    proto method cos(|$) {*}
    multi method cos() {
        pir::perl6_box_num__PN(pir::cos__NN(pir::repr_unbox_num__NP(self)));
    }
    proto method acos(|$) {*}
    multi method acos() {
        pir::perl6_box_num__PN(pir::acos__NN(pir::repr_unbox_num__NP(self)));
    }
    proto method tan(|$) {*}
    multi method tan() {
        pir::perl6_box_num__PN(pir::tan__NN(pir::repr_unbox_num__NP(self)));
    }
    proto method atan(|$) {*}
    multi method atan() {
        pir::perl6_box_num__PN(pir::atan__NN(pir::repr_unbox_num__NP(self)));
    }
    proto method sec(|$) {*}
    multi method sec() {
        pir::perl6_box_num__PN(pir::sec__NN(pir::repr_unbox_num__NP(self)));
    }
    proto method asec(|$) {*}
    multi method asec() {
        pir::perl6_box_num__PN(pir::asec__NN(pir::repr_unbox_num__NP(self)));
    }
    proto method sinh(|$) {*}
    multi method sinh() {
        pir::perl6_box_num__PN(pir::sinh__NN(pir::repr_unbox_num__NP(self)));
    }
    proto method cosh(|$) {*}
    multi method cosh() {
        pir::perl6_box_num__PN(pir::cosh__NN(pir::repr_unbox_num__NP(self)));
    }
    proto method tanh(|$) {*}
    multi method tanh() {
        pir::perl6_box_num__PN(pir::tanh__NN(pir::repr_unbox_num__NP(self)));
    }
    proto method sech(|$) {*}
    multi method sech() {
        pir::perl6_box_num__PN(pir::sech__NN(pir::repr_unbox_num__NP(self)));
    }
}

multi prefix:<++>(Num \$a is rw) {   # XXX
    $a = nqp::p6box_n(nqp::add_n(nqp::unbox_n($a), 1))
}
multi prefix:<-->(Num \$a is rw) {   # XXX
    $a = nqp::p6box_n(nqp::sub_n(nqp::unbox_n($a), 1))
}
multi postfix:<++>(Num \$a is rw) {  # XXX
    my $b = $a;
    $a = nqp::p6box_n(nqp::add_n(nqp::unbox_n($a), 1));
    $b
}
multi postfix:<-->(Num \$a is rw) {  # XXX
    my $b = $a;
    $a = nqp::p6box_n(nqp::sub_n(nqp::unbox_n($a), 1));
    $b
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

multi infix:<**>(Num \$a, Num \$b) {
    nqp::p6box_n(nqp::pow_n(nqp::unbox_n($a), nqp::unbox_n($b)))
}


multi infix:<cmp>(Num \$a, Num \$b) {
    nqp::p6box_n(nqp::cmp_n(nqp::unbox_n($a), nqp::unbox_n($b)))
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

