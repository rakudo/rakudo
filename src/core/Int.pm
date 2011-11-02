my class Rat { ... }

my class Int {
    multi method perl(Int:D:) {
        self.Str;
    }
    multi method Bool(Int:D:) {
        nqp::p6bool(nqp::isne_i(nqp::unbox_i(self), 0))
    }
    
    method Int() { self }
    
    multi method Str(Int:D:) {
        nqp::p6box_s(nqp::unbox_i(self));
    }
    
    method Num(Int:D:) {
        nqp::p6box_n(nqp::unbox_i(self));
    }

    method Rat(Int:D: $?) {
        Rat.new(self, 1);
    }

    method abs(Int:D:) {
        nqp::p6box_i(nqp::abs_i(nqp::unbox_i(self)));
    }

    method Bridge(Int:D:) {
        nqp::p6box_n(nqp::unbox_i(self));
    }

    method chr(Int:D:) {
        nqp::p6box_s(nqp::chr(nqp::unbox_i(self)));
    }

    method succ(Int:D:) { self + 1 }

    method pred(Int:D:) { self - 1 }

    method sqrt(Int:D:) { self.Num.sqrt }

    method base(Cool $base) {
        fail("base must be between 2 and 36, got $base") unless 2 <= $base <= 36;
        my int $b = $base.Int;
        my @conversion = qw/0 1 2 3 4 5 6 7 8 9
                            A B C D E F G H I J
                            K L M N O P Q R S T
                            U V W X Y Z/;
        my @res;
        my int $n = self.abs;
        repeat {
            push @res, @conversion[$n % $b];
            $n = $n div $b;
        } while $n > 0;
        push @res, '-' if self < 0;
        join '', @res.reverse;
    }
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

multi prefix:<->(Int \$a) returns Int {
    nqp::p6box_i(nqp::neg_i(nqp::unbox_i($a)))
}
multi prefix:<->(int $a) returns int {
    nqp::neg_i($a)
}

multi prefix:<abs>(Int:D \$a) returns Int {
    nqp::p6box_i(nqp::abs_i(nqp::unbox_i($a)))
}
multi prefix:<->(int $a) returns int {
    nqp::abs_i($a)
}

multi infix:<+>(Int:D \$a, Int:D \$b) returns Int {
    nqp::p6box_i(nqp::add_i(nqp::unbox_i($a), nqp::unbox_i($b)))
}
multi infix:<+>(int $a, int $b) returns int {
    nqp::add_i($a, $b)
}

multi infix:<->(Int:D \$a, Int:D \$b) returns Int {
    nqp::p6box_i(nqp::sub_i(nqp::unbox_i($a), nqp::unbox_i($b)))
}
multi infix:<->(int $a, int $b) returns int {
    nqp::sub_i($a, $b)
}

multi infix:<*>(Int:D \$a, Int:D \$b) returns Int {
    nqp::p6box_i(nqp::mul_i(nqp::unbox_i($a), nqp::unbox_i($b)))
}
multi infix:<*>(int $a, int $b) returns int {
    nqp::mul_i($a, $b)
}

multi infix:<div>(Int:D \$a, Int:D \$b) returns Int {
    nqp::p6box_i(nqp::div_i(nqp::unbox_i($a), nqp::unbox_i($b)))
}
multi infix:<div>(int $a, int $b) returns int {
    nqp::div_i($a, $b)
}

multi infix:<%>(Int:D \$a, Int:D \$b) returns Int {
    nqp::p6box_i(nqp::mod_i(nqp::unbox_i($a), nqp::unbox_i($b)))
}
multi infix:<%>(int $a, int $b) returns int {
    nqp::mod_i($a, $b)
}

multi infix:<**>(Int:D \$a, Int:D \$b) {
    if $b >= 0 {
        nqp::p6box_i(nqp::pow_n(nqp::unbox_i($a), nqp::unbox_i($b)))
    } else {
        nqp::p6box_n(nqp::pow_n(nqp::unbox_i($a), nqp::unbox_i($b)))
    }
}

multi infix:<lcm>(Int:D \$a, Int:D \$b) returns Int {
    nqp::p6box_i(nqp::lcm_i(nqp::unbox_i($a), nqp::unbox_i($b)))
}
multi infix:<lcm>(int $a, int $b) returns int {
    nqp::lcm_i($a, $b)
}

multi infix:<gcd>(Int:D \$a, Int:D \$b) returns Int {
    nqp::p6box_i(nqp::gcd_i(nqp::unbox_i($a), nqp::unbox_i($b)))
}
multi infix:<gcd>(int $a, int $b) returns int {
    nqp::gcd_i($a, $b)
}

multi infix:<cmp>(Int:D \$a, Int:D \$b) {
    nqp::p6box_i(nqp::cmp_i(nqp::unbox_i($a), nqp::unbox_i($b)))
}
multi infix:<cmp>(int $a, int $b) {
    nqp::cmp_i($a, $b)
}

multi infix:«<=>»(Int:D \$a, Int:D \$b) {
    nqp::p6box_i(nqp::cmp_i(nqp::unbox_i($a), nqp::unbox_i($b)))
}
multi infix:«<=>»(int $a, int $b) {
    nqp::cmp_i($a, $b)
}

multi infix:<===>(Int:D \$a, Int:D \$b) {
    nqp::p6bool(nqp::iseq_i(nqp::unbox_i($a), nqp::unbox_i($b)))
}
multi infix:<===>(int $a, int $b) {
    # hey, the optimizer is smart enough to figure that one out for us, no?
    $a == $b
}

multi infix:<==>(Int:D \$a, Int:D \$b) {
    nqp::p6bool(nqp::iseq_i(nqp::unbox_i($a), nqp::unbox_i($b)))
}
multi infix:<==>(int $a, int $b) {
    nqp::p6bool(nqp::iseq_i($a, $b))
}

multi infix:<!=>(Int:D \$a, Int:D \$b) {
    nqp::p6bool(nqp::isne_i(nqp::unbox_i($a), nqp::unbox_i($b)))
}
multi infix:<!=>(int \$a, int \$b) {
    nqp::p6bool(nqp::isne_i($a, $b))
}

multi infix:«<»(Int:D \$a, Int:D \$b) {
    nqp::p6bool(nqp::islt_i(nqp::unbox_i($a), nqp::unbox_i($b)))
}
multi infix:«<»(int $a, int $b) {
    nqp::p6bool(nqp::islt_i($a, $b))
}

multi infix:«<=»(Int:D \$a, Int:D \$b) {
    nqp::p6bool(nqp::isle_i(nqp::unbox_i($a), nqp::unbox_i($b)))
}
multi infix:«<=»(int $a, int $b) {
    nqp::p6bool(nqp::isle_i($a, $b))
}

multi infix:«>»(Int:D \$a, Int:D \$b) {
    nqp::p6bool(nqp::isgt_i(nqp::unbox_i($a), nqp::unbox_i($b)))
}
multi infix:«>»(int $a, int $b) {
    nqp::p6bool(nqp::isgt_i($a, $b))
}

multi infix:«>=»(Int:D \$a, Int:D \$b) {
    nqp::p6bool(nqp::isge_i(nqp::unbox_i($a), nqp::unbox_i($b)))
}
multi infix:«>=»(int $a, int $b) {
    nqp::p6bool(nqp::isge_i($a, $b))
}

multi infix:<+|>(Int:D \$a, Int:D \$b) {
    nqp::p6box_i(nqp::bitor_i(nqp::unbox_i($a), nqp::unbox_i($b)))
}
multi infix:<+|>(int $a, int $b) {
    nqp::bitor_i($a, $b)
}

multi infix:<+&>(Int:D \$a, Int:D \$b) {
    nqp::p6box_i(nqp::bitand_i(nqp::unbox_i($a), nqp::unbox_i($b)))
}
multi infix:<+&>(int $a, int $b) {
    nqp::bitand_i($a, $b)
}

multi infix:<+^>(Int:D \$a, Int:D \$b) {
    nqp::p6box_i(nqp::bitxor_i(nqp::unbox_i($a), nqp::unbox_i($b)))
}
multi infix:<+^>(int $a, int $b) {
    nqp::bitxor_i($a, $b);
}

multi infix:«+<»(Int:D \$a, Int:D \$b) {
    nqp::p6box_i(nqp::bitshiftl_i(nqp::unbox_i($a), nqp::unbox_i($b)))
}
multi infix:«+<»(int $a, int $b) {
    nqp::bitshiftl_i($a, $b);
}

multi infix:«+>»(Int:D \$a, Int:D \$b) {
    nqp::p6box_i(nqp::bitshiftr_i(nqp::unbox_i($a), nqp::unbox_i($b)))
}
multi infix:«+>»(int $a, int $b) {
    nqp::bitshiftr_i($a, $b)
}

multi prefix:<+^>(Int:D \$a) {
    nqp::p6box_i(nqp::bitneg_i(nqp::unbox_i($a)))
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
