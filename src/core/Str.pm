my class Str {
    method Bool() { self ne '' && self ne '0' }
    
    multi method Str(Str:D:) { self }
    
    method Int() {
        nqp::p6box_i(nqp::unbox_s(self));
    }
    
    method Num() {
        nqp::p6box_n(nqp::unbox_s(self));
    }

    method Numeric() { self.Num }

    multi method ACCEPTS(Str:D: $other) { $other eq self }

    # XXX: need to translate escapes
    multi method perl(Str:D:) { "'" ~ self ~ "'" }

}

multi infix:<cmp>(Str \$a, Str \$b) {
    nqp::p6box_i(pir::cmp__ISS(nqp::unbox_s($a), nqp::unbox_s($b)))
}

proto infix:<x>(|$) {*}
multi infix:<x>(Str $s, Int $repetition) {
    nqp::p6box_s(pir::repeat__SSI(nqp::unbox_s($s), nqp::unbox_i($repetition)))
}
multi infix:<x>($s, $repetition) { $s.Stringy x $repetition.Numeric }

multi prefix:<~>(Str \$a) { $a }

multi infix:<~>(Str \$a, Str \$b) {
    nqp::p6box_s(pir::concat__SSS(nqp::unbox_s($a), nqp::unbox_s($b)))
}

multi infix:<eq>(Str \$a, Str \$b) {
    nqp::p6bool(nqp::iseq_s(nqp::unbox_s($a), nqp::unbox_s($b)))
}

multi infix:<ne>(Str \$a, Str \$b) {
    nqp::p6bool(nqp::isne_s(nqp::unbox_s($a), nqp::unbox_s($b)))
}

multi infix:<lt>(Str \$a, Str \$b) {
    nqp::p6bool(nqp::islt_s(nqp::unbox_s($a), nqp::unbox_s($b)))
}

multi infix:<le>(Str \$a, Str \$b) {
    nqp::p6bool(nqp::isle_s(nqp::unbox_s($a), nqp::unbox_s($b)))
}

multi infix:<gt>(Str \$a, Str \$b) {
    nqp::p6bool(nqp::isgt_s(nqp::unbox_s($a), nqp::unbox_s($b)))
}

multi infix:<ge>(Str \$a, Str \$b) {
    nqp::p6bool(nqp::isge_s(nqp::unbox_s($a), nqp::unbox_s($b)))
}


multi infix:<~|>(Str \$a, Str \$b) {
    nqp::p6box_s(pir::bors__SSS(nqp::unbox_s($a), nqp::unbox_s($b)))
}

multi infix:<~&>(Str \$a, Str \$b) {
    nqp::p6box_s(pir::bands__SSS(nqp::unbox_s($a), nqp::unbox_s($b)))
}

multi infix:<~^>(Str \$a, Str \$b) {
    nqp::p6box_s(pir::bxors__SSS(nqp::unbox_s($a), nqp::unbox_s($b)))
}

multi prefix:<~^>(Str \$a) {
    fail "prefix:<~^> NYI";   # XXX
}
