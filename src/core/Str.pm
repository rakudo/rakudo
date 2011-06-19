my class Str {
    method Bool() { self ne '' && self ne '0' }
    
    multi method Str(Str:D:) { self }
    
    method Int() {
        pir::perl6_box_int__PI(pir::repr_unbox_str__SP(self));
    }
    
    method Num() {
        pir::perl6_box_num__PN(pir::repr_unbox_str__SP(self));
    }

    method Numeric() { self.Num }

    multi method ACCEPTS(Str:D: $other) { $other eq self }

    # XXX: need to translate escapes
    multi method perl(Str:D:) { "'" ~ self ~ "'" }

}

multi infix:<cmp>(Str \$a, Str \$b) {
    pir::perl6_box_int__PI(
        pir::cmp__ISS(
            pir::repr_unbox_str__SP($a),
            pir::repr_unbox_str__SP($b)))
}

proto infix:<x>(|$) {*}
multi infix:<x>(Str $s, Int $repetition) {
    pir::perl6_box_str__PS(
        pir::repeat__SSI(
            pir::repr_unbox_str__SP($s),
            pir::repr_unbox_int__IP($repetition),
        )
    );
}
multi infix:<x>($s, $repetition) { $s.Stringy x $repetition.Numeric }

multi prefix:<~>(Str \$a) { $a }

multi infix:<~>(Str \$a, Str \$b) {
    pir::perl6_box_str__PS(
        pir::concat__SSS(
            pir::repr_unbox_str__SP($a),
            pir::repr_unbox_str__SP($b)))
}

multi infix:<eq>(Str \$a, Str \$b) {
    pir::perl6_booleanize__PI(
        pir::iseq__ISS(
            pir::repr_unbox_str__SP($a),
            pir::repr_unbox_str__SP($b)))
}

multi infix:<ne>(Str \$a, Str \$b) {
    pir::perl6_booleanize__PI(
        pir::isne__ISS(
            pir::repr_unbox_str__SP($a),
            pir::repr_unbox_str__SP($b)))
}

multi infix:<lt>(Str \$a, Str \$b) {
    pir::perl6_booleanize__PI(
        pir::islt__ISS(
            pir::repr_unbox_str__SP($a),
            pir::repr_unbox_str__SP($b)))
}

multi infix:<le>(Str \$a, Str \$b) {
    pir::perl6_booleanize__PI(
        pir::isle__ISS(
            pir::repr_unbox_str__SP($a),
            pir::repr_unbox_str__SP($b)))
}

multi infix:<gt>(Str \$a, Str \$b) {
    pir::perl6_booleanize__PI(
        pir::isgt__ISS(
            pir::repr_unbox_str__SP($a),
            pir::repr_unbox_str__SP($b)))
}

multi infix:<ge>(Str \$a, Str \$b) {
    pir::perl6_booleanize__PI(
        pir::isge__ISS(
            pir::repr_unbox_str__SP($a),
            pir::repr_unbox_str__SP($b)))
}


multi infix:<~|>(Str \$a, Str \$b) {
    pir::perl6_box_str__PS(
        pir::bors__SSS(
            pir::repr_unbox_str__SP($a),
            pir::repr_unbox_str__SP($b)))
}

multi infix:<~&>(Str \$a, Str \$b) {
    pir::perl6_box_str__PS(
        pir::bands__SSS(
            pir::repr_unbox_str__SP($a),
            pir::repr_unbox_str__SP($b)))
}

multi infix:<~^>(Str \$a, Str \$b) {
    pir::perl6_box_str__PS(
        pir::bxors__SSS(
            pir::repr_unbox_str__SP($a),
            pir::repr_unbox_str__SP($b)))
}

multi prefix:<~^>(Str \$a) {
    fail "prefix:<~^> NYI";   # XXX
}
