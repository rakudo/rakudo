my class Int {
    method Bool() {
        nqp::p6bool(
            nqp::isne_i(nqp::unbox_i(self), nqp::unbox_i(0)))
    }
    
    method Int() { self }
    
    multi method Str(Int:D:) {
        nqp::p6box_s(nqp::unbox_i(self));
    }
    
    method Num() {
        nqp::p6box_n(nqp::unbox_i(self));
    }

    method abs() {
        nqp::p6box_i(nqp::abs_i(nqp::unbox_i(self)));
    }

    method Bridge() {
        self.Num;
    }

    method chr() {
        nqp::p6box_s(pir::chr(nqp::unbox_i(self)));
    }

    method succ(Int:D:) { self + 1 }

    method pred(Int:D:) { self - 1 }
}

multi prefix:<++>(Int:D \$a is rw) {   # XXX
    $a = nqp::p6box_i(nqp::add_i(nqp::unbox_i($a), 1))
}
multi prefix:<-->(Int:D \$a is rw) {   # XXX
    $a = nqp::p6box_i(nqp::sub_i(nqp::unbox_i($a), 1))
}
multi postfix:<++>(Int:D \$a is rw) {  # XXX
    my $b = $a;
    $a = nqp::p6box_i(nqp::add_i(nqp::unbox_i($a), 1));
    $b
}
multi postfix:<-->(Int:D \$a is rw) {  # XXX
    my $b = $a;
    $a = nqp::p6box_i(nqp::sub_i(nqp::unbox_i($a), 1));
    $b
}

multi prefix:<->(Int \$a) {
    nqp::p6box_i(nqp::neg_i(nqp::unbox_i($a)))
}

multi prefix:<abs>(Int \$a) {
    nqp::p6box_i(nqp::abs_i(nqp::unbox_i($a)))
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
    nqp::p6box_i(nqp::mod_i(nqp::unbox_i($a), nqp::unbox_i($b)))
}

multi infix:<**>(Int \$a, Int \$b) {
    nqp::p6box_i(nqp::pow_n(nqp::unbox_i($a), nqp::unbox_i($b)))
}

multi infix:<lcm>(Int \$a, Int \$b) {
    nqp::p6box_i(nqp::lcm_i(nqp::unbox_i($a), nqp::unbox_i($b)))
}

multi infix:<gcd>(Int \$a, Int \$b) {
    nqp::p6box_i(nqp::gcd_i(nqp::unbox_i($a), nqp::unbox_i($b)))
}

multi infix:<cmp>(Int \$a, Int \$b) {
    nqp::p6box_i(nqp::cmp_i(nqp::unbox_i($a), nqp::unbox_i($b)))
}

multi infix:<===>(Int \$a, Int \$b) {
    nqp::p6bool(nqp::iseq_i(nqp::unbox_i($a), nqp::unbox_i($b)))
}

multi infix:<==>(Int \$a, Int \$b) {
    nqp::p6bool(nqp::iseq_i(nqp::unbox_i($a), nqp::unbox_i($b)))
}

multi infix:<!=>(Int \$a, Int \$b) {
    nqp::p6bool(nqp::isne_i(nqp::unbox_i($a), nqp::unbox_i($b)))
}

multi infix:«<»(Int \$a, Int \$b) {
    nqp::p6bool(nqp::islt_i(nqp::unbox_i($a), nqp::unbox_i($b)))
}

multi infix:«<=»(Int \$a, Int \$b) {
    nqp::p6bool(nqp::isle_i(nqp::unbox_i($a), nqp::unbox_i($b)))
}

multi infix:«>»(Int \$a, Int \$b) {
    nqp::p6bool(nqp::isgt_i(nqp::unbox_i($a), nqp::unbox_i($b)))
}

multi infix:«>=»(Int \$a, Int \$b) {
    nqp::p6bool(nqp::isge_i(nqp::unbox_i($a), nqp::unbox_i($b)))
}

multi infix:<+|>(Int \$a, Int \$b) {
    nqp::p6box_i(pir::bor__III(nqp::unbox_i($a), nqp::unbox_i($b)))
}

multi infix:<+&>(Int \$a, Int \$b) {
    nqp::p6box_i(pir::band__III(nqp::unbox_i($a), nqp::unbox_i($b)))
}

multi infix:<+^>(Int \$a, Int \$b) {
    nqp::p6box_i(pir::bxor__III(nqp::unbox_i($a), nqp::unbox_i($b)))
}

multi infix:«+<»(Int \$a, Int \$b) {
    nqp::p6box_i(pir::shl__III(nqp::unbox_i($a), nqp::unbox_i($b)))
}

multi infix:«+>»(Int \$a, Int \$b) {
    nqp::p6box_i(pir::shr__III(nqp::unbox_i($a), nqp::unbox_i($b)))
}

multi prefix:<+^>(Int \$a) {
    nqp::p6box_i(pir::bnot__II(nqp::unbox_i($a)))
}

