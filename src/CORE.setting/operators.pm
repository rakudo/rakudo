sub infix:<=>(Mu \$a, Mu \$b) {
    pir::perl6_container_store__0PP($a, pir::perl6_decontainerize__PP($b))
}

proto infix:<+>(|$) { * }
multi infix:<+>(Int $a, Int $b) {
    pir::repr_box_int__PIP(
        pir::add__III(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b)),
        Int)
}

proto infix:<->(|$) { * }
multi infix:<->(Int $a, Int $b) {
    pir::repr_box_int__PIP(
        pir::sub__III(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b)),
        Int)
}

proto infix:<*>(|$) { * }
multi infix:<*>(Int $a, Int $b) {
    pir::repr_box_int__PIP(
        pir::mul__III(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b)),
        Int)
}

proto infix:</>(|$) { * }
multi infix:</>(Int $a, Int $b) {
    pir::repr_box_int__PIP(
        pir::div__III(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b)),
        Int)
}

proto infix:<~>(|$) { * }
multi infix:<~>(Str $a, Str $b) {
    pir::repr_box_str__PSP(
        pir::concat__SSS(pir::repr_unbox_str__SP($a), pir::repr_unbox_str__SP($b)),
        Str)
}

proto infix:<==>(|$) { * }
multi infix:<==>(Int $a, Int $b) {
    pir::perl6_booleanize__PI(
        pir::iseq__III(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b)))
}

proto infix:<!=>(|$) { * }
multi infix:<!=>(Int $a, Int $b) {
    pir::perl6_booleanize__PI(
        pir::isne__III(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b)))
}

proto infix:«<»(|$) { * }
multi infix:«<»(Int $a, Int $b) {
    pir::perl6_booleanize__PI(
        pir::islt__III(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b)))
}

proto infix:«<=»(|$) { * }
multi infix:«<=»(Int $a, Int $b) {
    pir::perl6_booleanize__PI(
        pir::isle__III(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b)))
}

proto infix:«>»(|$) { * }
multi infix:«>»(Int $a, Int $b) {
    pir::perl6_booleanize__PI(
        pir::isgt__III(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b)))
}

proto infix:«>=»(|$) { * }
multi infix:«>=»(Int $a, Int $b) {
    pir::perl6_booleanize__PI(
        pir::isge__III(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b)))
}

proto infix:<eq>(|$) { * }
multi infix:<eq>(Str $a, Str $b) {
    pir::perl6_booleanize__PI(
        pir::iseq__ISS(pir::repr_unbox_str__SP($a), pir::repr_unbox_str__SP($b)))
}

proto infix:<ne>(|$) { * }
multi infix:<ne>(Str $a, Str $b) {
    pir::perl6_booleanize__PI(
        pir::isne__ISS(pir::repr_unbox_str__SP($a), pir::repr_unbox_str__SP($b)))
}

proto infix:<lt>(|$) { * }
multi infix:<lt>(Str $a, Str $b) {
    pir::perl6_booleanize__PI(
        pir::islt__ISS(pir::repr_unbox_str__SP($a), pir::repr_unbox_str__SP($b)))
}

proto infix:<le>(|$) { * }
multi infix:<le>(Str $a, Str $b) {
    pir::perl6_booleanize__PI(
        pir::isle__ISS(pir::repr_unbox_str__SP($a), pir::repr_unbox_str__SP($b)))
}

proto infix:<gt>(|$) { * }
multi infix:<gt>(Str $a, Str $b) {
    pir::perl6_booleanize__PI(
        pir::isgt__ISS(pir::repr_unbox_str__SP($a), pir::repr_unbox_str__SP($b)))
}

proto infix:<ge>(|$) { * }
multi infix:<ge>(Str $a, Str $b) {
    pir::perl6_booleanize__PI(
        pir::isge__ISS(pir::repr_unbox_str__SP($a), pir::repr_unbox_str__SP($b)))
}
