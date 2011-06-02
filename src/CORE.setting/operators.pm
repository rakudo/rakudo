sub infix:<=>(Mu \$a, Mu \$b) {
    pir::perl6_container_store__0PP($a, pir::perl6_decontainerize__PP($b))
}

proto infix:<+>(Mu |$) { * }
multi infix:<+>(Int $a, Int $b) {
    pir::repr_box_int__PIP(
        pir::add__III(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b)),
        Int)
}

proto infix:<->(Mu |$) { * }
multi infix:<->(Int $a, Int $b) {
    pir::repr_box_int__PIP(
        pir::sub__III(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b)),
        Int)
}

proto infix:<*>(Mu |$) { * }
multi infix:<*>(Int $a, Int $b) {
    pir::repr_box_int__PIP(
        pir::mul__III(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b)),
        Int)
}

proto infix:</>(Mu |$) { * }
multi infix:</>(Int $a, Int $b) {
    pir::repr_box_int__PIP(
        pir::div__III(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b)),
        Int)
}

proto infix:<~>(Mu |$) { * }
multi infix:<~>(Str $a, Str $b) {
    pir::repr_box_str__PSP(
        pir::concat__SSS(pir::repr_unbox_str__SP($a), pir::repr_unbox_str__SP($b)),
        Str)
}
