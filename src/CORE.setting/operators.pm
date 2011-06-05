sub infix:<=>(Mu \$a, Mu \$b) {
    pir::perl6_container_store__0PP($a, pir::perl6_decontainerize__PP($b))
}

proto infix:<+>(|$) { * }
multi infix:<+>(Int \$a, Int \$b) {
    pir::perl6_box_int__PI(
        pir::add__III(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b)))
}

proto infix:<->(|$) { * }
multi infix:<->(Int \$a, Int \$b) {
    pir::perl6_box_int__PI(
        pir::sub__III(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b)))
}

proto infix:<*>(|$) { * }
multi infix:<*>(Int \$a, Int \$b) {
    pir::perl6_box_int__PI(
        pir::mul__III(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b)))
}

proto infix:</>(|$) { * }
multi infix:</>(Int \$a, Int \$b) {
    pir::perl6_box_int__PI(
        pir::div__III(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b)))
}

proto infix:<~>(|$) { * }
multi infix:<~>(Str \$a, Str \$b) {
    pir::perl6_box_str__PS(
        pir::concat__SSS(pir::repr_unbox_str__SP($a), pir::repr_unbox_str__SP($b)))
}

proto infix:<==>(|$) { * }
multi infix:<==>(Int \$a, Int \$b) {
    pir::perl6_booleanize__PI(
        pir::iseq__III(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b)))
}

proto infix:<!=>(|$) { * }
multi infix:<!=>(Int \$a, Int \$b) {
    pir::perl6_booleanize__PI(
        pir::isne__III(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b)))
}

proto infix:«<»(|$) { * }
multi infix:«<»(Int \$a, Int \$b) {
    pir::perl6_booleanize__PI(
        pir::islt__III(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b)))
}

proto infix:«<=»(|$) { * }
multi infix:«<=»(Int \$a, Int \$b) {
    pir::perl6_booleanize__PI(
        pir::isle__III(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b)))
}

proto infix:«>»(|$) { * }
multi infix:«>»(Int \$a, Int \$b) {
    pir::perl6_booleanize__PI(
        pir::isgt__III(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b)))
}

proto infix:«>=»(|$) { * }
multi infix:«>=»(Int \$a, Int \$b) {
    pir::perl6_booleanize__PI(
        pir::isge__III(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b)))
}

proto infix:<eq>(|$) { * }
multi infix:<eq>(Str \$a, Str \$b) {
    pir::perl6_booleanize__PI(
        pir::iseq__ISS(pir::repr_unbox_str__SP($a), pir::repr_unbox_str__SP($b)))
}

proto infix:<ne>(|$) { * }
multi infix:<ne>(Str \$a, Str \$b) {
    pir::perl6_booleanize__PI(
        pir::isne__ISS(pir::repr_unbox_str__SP($a), pir::repr_unbox_str__SP($b)))
}

proto infix:<lt>(|$) { * }
multi infix:<lt>(Str \$a, Str \$b) {
    pir::perl6_booleanize__PI(
        pir::islt__ISS(pir::repr_unbox_str__SP($a), pir::repr_unbox_str__SP($b)))
}

proto infix:<le>(|$) { * }
multi infix:<le>(Str \$a, Str \$b) {
    pir::perl6_booleanize__PI(
        pir::isle__ISS(pir::repr_unbox_str__SP($a), pir::repr_unbox_str__SP($b)))
}

proto infix:<gt>(|$) { * }
multi infix:<gt>(Str \$a, Str \$b) {
    pir::perl6_booleanize__PI(
        pir::isgt__ISS(pir::repr_unbox_str__SP($a), pir::repr_unbox_str__SP($b)))
}

proto infix:<ge>(|$) { * }
multi infix:<ge>(Str \$a, Str \$b) {
    pir::perl6_booleanize__PI(
        pir::isge__ISS(pir::repr_unbox_str__SP($a), pir::repr_unbox_str__SP($b)))
}

proto infix:<+|>(|$) { * }
multi infix:<+|>(Int \$a, Int \$b) {
    pir::perl6_box_int__PI(pir::bor__III(
        pir::repr_unbox_int__ip($a),
        pir::repr_unbox_int__ip($b)
    ));
}

proto infix:<+&>(|$) { * }
multi infix:<+&>(Int \$a, Int \$b) {
    pir::perl6_box_int__PI(pir::band__III(
        pir::repr_unbox_int__ip($a),
        pir::repr_unbox_int__ip($b)
    ));
}

proto infix:<+^>(|$) { * }
multi infix:<+^>(Int \$a, Int \$b) {
    pir::perl6_box_int__PI(pir::bxor__III(
        pir::repr_unbox_int__ip($a),
        pir::repr_unbox_int__ip($b)
    ));
}

proto infix:«+<»(|$) { * }
multi infix:«+<»(Int \$a, Int \$b) {
    pir::perl6_box_int__PI(pir::shl__III(
        pir::repr_unbox_int__ip($a),
        pir::repr_unbox_int__ip($b)
    ));
}

proto infix:«+>»(|$) { * }
multi infix:«+>»(Int \$a, Int \$b) {
    pir::perl6_box_int__PI(pir::shr__III(
        pir::repr_unbox_int__ip($a),
        pir::repr_unbox_int__ip($b)
    ));
}

proto prefix:<+^>(|$) { * }
multi prefix:<+^>(Int \$a) {
    pir::perl6_box_int__PI(pir::bnot__II(
        pir::repr_unbox_int__ip($a)
    ));
}

proto infix:<**>(|$) { * }
multi infix:<**>(Num \$a, Num \$b) {
    pir::perl6_box_num__PN(pir::pow__NNN(pir::repr_unbox_num__np($a), pir::repr_unbox_num__np($b)));
}
