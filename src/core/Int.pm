my class Rat { ... }

my class Int {
    multi method WHICH(Int:D:) {
        nqp::box_s(
            nqp::concat_s(
                nqp::concat_s(nqp::unbox_s(self.^name), '|'),
                nqp::tostr_I(self)
            ),
            ObjAt
        );
    }
    multi method perl(Int:D:) {
        self.Str;
    }
    multi method Bool(Int:D:) {
        nqp::p6bool(nqp::bool_I(self));
    }
    
    method Int() { self }
    
    multi method Str(Int:D:) {
        nqp::p6box_s(nqp::tostr_I(self));
    }
    
    method Num(Int:D:) {
        nqp::p6box_n(nqp::tonum_I(self));
    }

    method Rat(Int:D: $?) {
        Rat.new(self, 1);
    }
    method FatRat(Int:D: $?) {
        FatRat.new(self, 1);
    }

    method abs(Int:D:) {
        nqp::abs_I(self, Int)
    }

    method Bridge(Int:D:) {
        nqp::p6box_n(nqp::tonum_I(self));
    }

    method chr(Int:D:) {
        nqp::p6box_s(nqp::chr(nqp::unbox_i(self)));
    }

    method succ(Int:D:) { self + 1 }

    method pred(Int:D:) { self - 1 }

    method sqrt(Int:D:) { nqp::p6box_n(nqp::sqrt_n(nqp::tonum_I(self))) }

    method base(Int:D: Cool $base) {
        fail("base must be between 2 and 36, got $base") unless 2 <= $base <= 36;
        my int $b = nqp::unbox_i($base.Int);
        nqp::p6box_s(nqp::base_I(self, $b));
    }

    method floor(Int:D:) { self }
    method round(Int:D:) { self }
    method ceiling(Int:D:) { self }
}

multi prefix:<++>(Int:D \$a is rw) {   # XXX
    $a = nqp::add_I(nqp::p6decont($a), nqp::p6box_i(1), Int);
}
multi prefix:<-->(Int:D \$a is rw) {   # XXX
    $a = nqp::sub_I(nqp::p6decont($a), nqp::p6box_i(1), Int);
}
multi postfix:<++>(Int:D \$a is rw) {  # XXX
    my Int:D $b = $a;
    $a = nqp::add_I(nqp::p6decont($a), nqp::p6box_i(1), Int);
    $b
}
multi postfix:<-->(Int:D \$a is rw) {  # XXX
    my Int:D $b = $a;
    $a = nqp::sub_I(nqp::p6decont($a), nqp::p6box_i(1), Int);
    $b
}

multi prefix:<->(Int \$a) returns Int {
    nqp::neg_I(nqp::p6decont($a), Int);
}
multi prefix:<->(int $a) returns int {
    nqp::neg_i($a)
}

multi prefix:<abs>(Int:D \$a) returns Int:D {
    nqp::abs_I(nqp::p6decont($a), Int);
}
multi prefix:<abs>(int $a) returns int {
    nqp::abs_i($a)
}

multi infix:<+>(Int:D \$a, Int:D \$b) returns Int:D {
    nqp::add_I(nqp::p6decont($a), nqp::p6decont($b), Int);
}
multi infix:<+>(int $a, int $b) returns int {
    nqp::add_i($a, $b)
}

multi infix:<->(Int:D \$a, Int:D \$b) returns Int:D {
    nqp::sub_I(nqp::p6decont($a), nqp::p6decont($b), Int);
}
multi infix:<->(int $a, int $b) returns int {
    nqp::sub_i($a, $b)
}

multi infix:<*>(Int:D \$a, Int:D \$b) returns Int {
    nqp::mul_I(nqp::p6decont($a), nqp::p6decont($b), Int);
}
multi infix:<*>(int $a, int $b) returns int {
    nqp::mul_i($a, $b)
}

multi infix:<div>(Int:D \$a, Int:D \$b) returns Int {
    nqp::div_I(nqp::p6decont($a), nqp::p6decont($b), Int);
}
multi infix:<div>(int $a, int $b) returns int {
    nqp::div_i($a, $b)
}

multi infix:<%>(Int:D \$a, Int:D \$b) returns Int {
    nqp::mod_I(nqp::p6decont($a), nqp::p6decont($b), Int);
}
multi infix:<%>(int $a, int $b) returns int {
    nqp::mod_i($a, $b)
}

multi infix:<**>(Int:D \$a, Int:D \$b) {
    nqp::pow_I(nqp::p6decont($a), nqp::p6decont($b), Num, Int);
}

multi infix:<lcm>(Int:D \$a, Int:D \$b) returns Int {
    nqp::lcm_I(nqp::p6decont($a), nqp::p6decont($b), Int);
}
multi infix:<lcm>(int $a, int $b) returns int {
    nqp::lcm_i($a, $b)
}

multi infix:<gcd>(Int:D \$a, Int:D \$b) returns Int {
    nqp::gcd_I(nqp::p6decont($a), nqp::p6decont($b), Int);
}
multi infix:<gcd>(int $a, int $b) returns int {
    nqp::gcd_i($a, $b)
}

## Order enumeration, for cmp and <=>
my enum Order (:Increase(-1), :Same(0), :Decrease(1));

multi infix:<cmp>(Int:D \$a, Int:D \$b) {
    Order.(nqp::p6box_i(nqp::cmp_I(nqp::p6decont($a), nqp::p6decont($b))))
}
multi infix:<cmp>(int $a, int $b) {
    Order.(nqp::p6box_i(nqp::cmp_i($a, $b)))
}

multi infix:«<=>»(Int:D \$a, Int:D \$b) {
    Order.(nqp::p6box_i(nqp::cmp_I(nqp::p6decont($a), nqp::p6decont($b))))
}
multi infix:«<=>»(int $a, int $b) {
    Order.(nqp::p6box_i(nqp::cmp_i($a, $b)))
}

multi infix:<===>(Int:D \$a, Int:D \$b) {
    nqp::p6bool(nqp::iseq_I(nqp::p6decont($a), nqp::p6decont($b)))
}
multi infix:<===>(int $a, int $b) {
    # hey, the optimizer is smart enough to figure that one out for us, no?
    $a == $b
}

multi infix:<==>(Int:D \$a, Int:D \$b) {
    nqp::p6bool(nqp::iseq_I(nqp::p6decont($a), nqp::p6decont($b)))
}
multi infix:<==>(int $a, int $b) {
    nqp::p6bool(nqp::iseq_i($a, $b))
}

multi infix:<!=>(int $a, int $b) {
    nqp::p6bool(nqp::isne_i($a, $b))
}

multi infix:«<»(Int:D \$a, Int:D \$b) {
    nqp::p6bool(nqp::islt_I(nqp::p6decont($a), nqp::p6decont($b)))
}
multi infix:«<»(int $a, int $b) {
    nqp::p6bool(nqp::islt_i($a, $b))
}

multi infix:«<=»(Int:D \$a, Int:D \$b) {
    nqp::p6bool(nqp::isle_I(nqp::p6decont($a), nqp::p6decont($b)))
}
multi infix:«<=»(int $a, int $b) {
    nqp::p6bool(nqp::isle_i($a, $b))
}

multi infix:«>»(Int:D \$a, Int:D \$b) {
    nqp::p6bool(nqp::isgt_I(nqp::p6decont($a), nqp::p6decont($b)))
}
multi infix:«>»(int $a, int $b) {
    nqp::p6bool(nqp::isgt_i($a, $b))
}

multi infix:«>=»(Int:D \$a, Int:D \$b) {
    nqp::p6bool(nqp::isge_I(nqp::p6decont($a), nqp::p6decont($b)))
}
multi infix:«>=»(int $a, int $b) {
    nqp::p6bool(nqp::isge_i($a, $b))
}

multi infix:<+|>(Int:D \$a, Int:D \$b) {
    nqp::bitor_I(nqp::p6decont($a), nqp::p6decont($b), Int)
}
multi infix:<+|>(int $a, int $b) {
    nqp::bitor_i($a, $b)
}

multi infix:<+&>(Int:D \$a, Int:D \$b) {
    nqp::bitand_I(nqp::p6decont($a), nqp::p6decont($b), Int)
}
multi infix:<+&>(int $a, int $b) {
    nqp::bitand_i($a, $b)
}

multi infix:<+^>(Int:D \$a, Int:D \$b) {
    nqp::bitxor_I(nqp::p6decont($a), nqp::p6decont($b), Int)
}
multi infix:<+^>(int $a, int $b) {
    nqp::bitxor_i($a, $b);
}

multi infix:«+<»(Int:D \$a, Int:D \$b) returns Int:D {
    nqp::bitshiftl_I(nqp::p6decont($a), nqp::unbox_i($b), Int)
}
multi infix:«+<»(int $a, int $b) {
    nqp::bitshiftl_i($a, $b);
}

multi infix:«+>»(Int:D \$a, Int:D \$b) returns Int:D {
    nqp::bitshiftr_I(nqp::p6decont($a), nqp::unbox_i($b), Int)
}
multi infix:«+>»(int $a, int $b) {
    nqp::bitshiftr_i($a, $b)
}

multi prefix:<+^>(Int:D \$a) {
    nqp::bitneg_I(nqp::p6decont($a), Int);
}
multi prefix:<+^>(int $a) {
    nqp::bitneg_i($a);
}

proto sub chr($) {*}
multi sub chr(Int:D  \$x) returns Str:D { $x.chr     }
multi sub chr(Cool \$x) returns Str:D { $x.Int.chr }
multi sub chr(int $x) returns str {
    nqp::chr($x);
}
