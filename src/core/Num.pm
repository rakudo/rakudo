
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
    $a = pir::perl6_box_num__PN(pir::inc__0N(pir::repr_unbox_num__NP($a)))
}
multi prefix:<-->(Num \$a is rw) {   # XXX
    $a = pir::perl6_box_num__PN(pir::dec__0N(pir::repr_unbox_num__NP($a)))
}
multi postfix:<++>(Num \$a is rw) {  # XXX
    my $b = $a;
    $a = pir::perl6_box_num__PN(pir::inc__0N(pir::repr_unbox_num__NP($a)));
    $b
}
multi postfix:<-->(Num \$a is rw) {  # XXX
    my $b = $a;
    $a = pir::perl6_box_num__PN(pir::dec__0N(pir::repr_unbox_num__NP($a)));
    $b
}


multi prefix:<->(Num \$a) {
    pir::perl6_box_num__PN(
        pir::neg__NN(pir::repr_unbox_num__NP($a)))
}

multi prefix:<abs>(Num \$a) {
    pir::perl6_box_num__PN(
        pir::abs__NN(pir::repr_unbox_num__NP($a)))
}

multi infix:<+>(Num \$a, Num \$b) {
    pir::perl6_box_num__PN(
        pir::add__NNN(
            pir::repr_unbox_num__NP($a), 
            pir::repr_unbox_num__NP($b)))
}

multi infix:<->(Num \$a, Num \$b) {
    pir::perl6_box_num__PN(
        pir::sub__NNN(
            pir::repr_unbox_num__NP($a), 
            pir::repr_unbox_num__NP($b)))
}

multi infix:<*>(Num \$a, Num \$b) {
    pir::perl6_box_num__PN(
        pir::mul__NNN(
            pir::repr_unbox_num__NP($a), 
            pir::repr_unbox_num__NP($b)))
}

multi infix:</>(Num \$a, Num \$b) {
    pir::perl6_box_num__PN(
        pir::div__NNN(
            pir::repr_unbox_num__NP($a), 
            pir::repr_unbox_num__NP($b)))
}

multi infix:<%>(Num \$a, Num \$b) {
    pir::perl6_box_num__PN(
        pir::mod__NNN(
            pir::repr_unbox_num__NP($a), 
            pir::repr_unbox_num__NP($b)))
}

multi infix:<**>(Num \$a, Num \$b) {
    pir::perl6_box_num__PN(
        pir::pow__NNN(
            pir::repr_unbox_num__np($a), 
            pir::repr_unbox_num__np($b)))
}

multi infix:<cmp>(Num \$a, Num \$b) {
    pir::perl6_box_int__PI(
        pir::cmp__INN(
            pir::repr_unbox_num__NP($a),
            pir::repr_unbox_num__NP($b)))
}

multi infix:<==>(Num \$a, Num \$b) {
    pir::perl6_booleanize__PI(
        pir::iseq__INN(
            pir::repr_unbox_num__NP($a), 
            pir::repr_unbox_num__NP($b)))
}

multi infix:<!=>(Num \$a, Num \$b) {
    pir::perl6_booleanize__PI(
        pir::isne__INN(
            pir::repr_unbox_num__NP($a), 
            pir::repr_unbox_num__NP($b)))
}

multi infix:«<»(Num \$a, Num \$b) {
    pir::perl6_booleanize__PI(
        pir::islt__INN(
            pir::repr_unbox_num__NP($a), 
            pir::repr_unbox_num__NP($b)))
}

multi infix:«<=»(Num \$a, Num \$b) {
    pir::perl6_booleanize__PI(
        pir::isle__INN(
            pir::repr_unbox_num__NP($a), 
            pir::repr_unbox_num__NP($b)))
}

multi infix:«>»(Num \$a, Num \$b) {
    pir::perl6_booleanize__PI(
        pir::isgt__INN(
            pir::repr_unbox_num__NP($a), 
            pir::repr_unbox_num__NP($b)))
}

multi infix:«>=»(Num \$a, Num \$b) {
    pir::perl6_booleanize__PI(
        pir::isge__INN(
            pir::repr_unbox_num__NP($a), 
            pir::repr_unbox_num__NP($b)))
}

