my class Int {
    method Bool() {
        pir::perl6_booleanize__PI(
            pir::isne__III(pir::repr_unbox_int__IP(self), pir::repr_unbox_int__IP(0)))
    }
    
    method Int() { self }
    
    multi method Str(Int:D:) {
        pir::perl6_box_str__PS(pir::repr_unbox_int__IP(self));
    }
    
    method Num() {
        pir::perl6_box_num__PN(pir::repr_unbox_int__IP(self));
    }

    method abs() {
        pir::perl6_box_int__PI(pir::abs__II(pir::repr_unbox_int__IP(self)));
    }

    method Bridge() {
        self.Num;
    }

    method chr() {
        pir::perl6_box_str__PS(pir::chr(pir::repr_unbox_int__IP(self)));
    }

    proto method succ(|$) {*}
    multi method succ(Int:D:) { self + 1 }
    multi method succ(Int:U:) {        1 }

    proto method pred(|$) {*}
    multi method pred(Int:D:) { self - 1 }
    multi method pred(Int:U:) {       -1 }
}

multi prefix:<++>(Int \$a is rw) {   # XXX
    $a = pir::perl6_box_int__PI(pir::inc__0I(pir::repr_unbox_int__IP($a)))
}
multi prefix:<-->(Int \$a is rw) {   # XXX
    $a = pir::perl6_box_int__PI(pir::dec__0I(pir::repr_unbox_int__IP($a)))
}
multi postfix:<++>(Int \$a is rw) {  # XXX
    my $b = $a;
    $a = pir::perl6_box_int__PI(pir::inc__0I(pir::repr_unbox_int__IP($a)));
    $b
}
multi postfix:<-->(Int \$a is rw) {  # XXX
    my $b = $a;
    $a = pir::perl6_box_int__PI(pir::dec__0I(pir::repr_unbox_int__IP($a)));
    $b
}

multi prefix:<->(Int \$a) {
    pir::perl6_box_int__PI(pir::neg__II(pir::repr_unbox_int__IP($a)))
}

multi prefix:<abs>(Int \$a) {
    pir::perl6_box_int__PI(pir::abs__II(pir::repr_unbox_int__IP($a)))
}

multi infix:<+>(Int \$a, Int \$b) {
    nqp::p6box_i(nqp::add_i(nqp::unbox_i($a), nqp::unbox_i($b)))
}

multi infix:<->(Int \$a, Int \$b) {
    nqp::p6box_i(nqp::sub_i(nqp::unbox_i($a), nqp::unbox_i($b)))
}

multi infix:<*>(Int \$a, Int \$b) {
    nqp::p6box_i(nqp::mul_i(nqp::unbox_i($a), nqp::unbox_i($b)))
}

multi infix:</>(Int \$a, Int \$b) {
    # XXX should really return a Rat
    $a.Num / $b.Num
}

multi infix:<div>(Int \$a, Int \$b) {
    nqp::p6box_i(nqp::div_i(nqp::unbox_i($a), nqp::unbox_i($b)))
}

multi infix:<%>(Int \$a, Int \$b) {
    pir::perl6_box_int__PI(
        pir::mod__III(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b)))
}

multi infix:<**>(Int \$a, Int \$b) {
    pir::perl6_box_int__PI(pir::set__IN(pir::pow__NNN(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b))));
}

multi infix:<lcm>(Int \$a, Int \$b) {
    pir::perl6_box_int__PI(
        pir::lcm__III(
            pir::repr_unbox_int__IP($a),
            pir::repr_unbox_int__IP($b)))
}

multi infix:<gcd>(Int \$a, Int \$b) {
    pir::perl6_box_int__PI(
        pir::gcd__III(
            pir::repr_unbox_int__IP($a),
            pir::repr_unbox_int__IP($b)))
}

multi infix:<cmp>(Int \$a, Int \$b) {
    pir::perl6_box_int__PI(
        pir::cmp__III(
            pir::repr_unbox_int__IP($a), 
            pir::repr_unbox_int__IP($b)))
}

multi infix:<==>(Int \$a, Int \$b) {
    pir::perl6_booleanize__PI(
        pir::iseq__III(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b)))
}

multi infix:<!=>(Int \$a, Int \$b) {
    pir::perl6_booleanize__PI(
        pir::isne__III(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b)))
}

multi infix:«<»(Int \$a, Int \$b) {
    pir::perl6_booleanize__PI(
        pir::islt__III(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b)))
}

multi infix:«<=»(Int \$a, Int \$b) {
    pir::perl6_booleanize__PI(
        pir::isle__III(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b)))
}

multi infix:«>»(Int \$a, Int \$b) {
    pir::perl6_booleanize__PI(
        pir::isgt__III(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b)))
}

multi infix:«>=»(Int \$a, Int \$b) {
    pir::perl6_booleanize__PI(
        pir::isge__III(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b)))
}

multi infix:<+|>(Int \$a, Int \$b) {
    pir::perl6_box_int__PI(pir::bor__III(
        pir::repr_unbox_int__ip($a),
        pir::repr_unbox_int__ip($b)
    ));
}

multi infix:<+&>(Int \$a, Int \$b) {
    pir::perl6_box_int__PI(pir::band__III(
        pir::repr_unbox_int__ip($a),
        pir::repr_unbox_int__ip($b)
    ));
}

multi infix:<+^>(Int \$a, Int \$b) {
    pir::perl6_box_int__PI(pir::bxor__III(
        pir::repr_unbox_int__ip($a),
        pir::repr_unbox_int__ip($b)
    ));
}

multi infix:«+<»(Int \$a, Int \$b) {
    pir::perl6_box_int__PI(pir::shl__III(
        pir::repr_unbox_int__ip($a),
        pir::repr_unbox_int__ip($b)
    ));
}

multi infix:«+>»(Int \$a, Int \$b) {
    pir::perl6_box_int__PI(pir::shr__III(
        pir::repr_unbox_int__ip($a),
        pir::repr_unbox_int__ip($b)
    ));
}

multi prefix:<+^>(Int \$a) {
    pir::perl6_box_int__PI(pir::bnot__II(
        pir::repr_unbox_int__ip($a)
    ));
}

