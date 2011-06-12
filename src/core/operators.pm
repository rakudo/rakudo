sub infix:<=>(Mu \$a, Mu \$b) {
    pir::perl6_container_store__0PP($a, pir::perl6_decontainerize__PP($b))
}

proto prefix:<?>(|$) { * }
multi prefix:<?>(Bool \$a) { $a }
multi prefix:<?>(Mu \$a) { $a.Bool }
proto prefix:<so>(|$) { * }
multi prefix:<so>(Bool \$a) { $a }
multi prefix:<so>(Mu \$a) { $a.Bool }

# XXX These should use Bool::True and Bool::False eventually.
proto prefix:<!>(|$) { *}
multi prefix:<!>(Bool \$a) { $a ?? pir::perl6_booleanize__PI(0) !! pir::perl6_booleanize__PI(1) }
multi prefix:<!>(Mu \$a) { $a.Bool ?? pir::perl6_booleanize__PI(0) !! pir::perl6_booleanize__PI(1) }

proto prefix:<~>(|$) { * }
multi prefix:<~>(\$a) { $a.Stringy }

proto infix:<+>(|$) { * }
multi infix:<+>(Int \$a, Int \$b) {
    pir::perl6_box_int__PI(
        pir::add__III(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b)))
}
multi infix:<+>(Num \$a, Num \$b) {
    pir::perl6_box_num__PN(
        pir::add__NNN(pir::repr_unbox_num__NP($a), pir::repr_unbox_num__NP($b)))
}
multi infix:<+>(\$a, \$b) {
    $a.Numeric + $b.Numeric
}
multi infix:<+>(Real \$a, Real \$b) {
    $a.Bridge + $b.Bridge
}

proto infix:<->(|$) { * }
multi infix:<->(Int \$a, Int \$b) {
    pir::perl6_box_int__PI(
        pir::sub__III(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b)))
}
multi infix:<->(Num \$a, Num \$b) {
    pir::perl6_box_num__PN(
        pir::sub__NNN(pir::repr_unbox_num__NP($a), pir::repr_unbox_num__NP($b)))
}
proto prefix:<+>(|$) {*}
multi prefix:<+>(Numeric \$a) { $a }
multi prefix:<+>(\$a) { $a.Numeric }

proto prefix:<->(|$) { * }
multi prefix:<->(Int \$a) {
    pir::perl6_box_int__PI(pir::neg__II(pir::repr_unbox_int__IP($a)))
}
multi prefix:<->(Num \$a) {
    pir::perl6_box_num__PN(pir::neg__NN(pir::repr_unbox_num__NP($a)))
}

proto prefix:<abs>(|$) { * }
multi prefix:<abs>(Int \$a) {
    pir::perl6_box_int__PI(pir::abs__II(pir::repr_unbox_int__IP($a)))
}
multi prefix:<abs>(Num \$a) {
    pir::perl6_box_num__PN(pir::abs__II(pir::repr_unbox_num__NP($a)))
}
multi prefix:<abs>(\$a) {
    abs $a.Numeric
}

proto infix:<*>(|$) { * }
multi infix:<*>(Int \$a, Int \$b) {
    pir::perl6_box_int__PI(
        pir::mul__III(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b)))
}
multi infix:<*>(Num \$a, Num \$b) {
    pir::perl6_box_num__PN(
        pir::mul__NNN(pir::repr_unbox_num__NP($a), pir::repr_unbox_num__NP($b)))
}
multi infix:<*>(\$a, \$b) {
    $a.Numeric * $b.Numeric
}
multi infix:<*>(Real \$a, Real \$b) {
    $a.Bridge * $b.Bridge
}

proto infix:</>(|$) { * }
multi infix:</>(Int \$a, Int \$b) {
    # XXX should really return a Rat
    $a.Num / $b.Num
}
multi infix:</>(Num \$a, Num \$b) {
    pir::perl6_box_num__PN(
        pir::div__NNN(pir::repr_unbox_num__NP($a), pir::repr_unbox_num__NP($b)))
}
multi infix:</>(\$a, \$b) {
    $a.Numeric / $b.Numeric
}
multi infix:</>(Real \$a, Real \$b) {
    $a.Bridge / $b.Bridge
}

proto infix:<div>(|$) { * }
multi infix:<div>(Int \$a, Int \$b) {
    pir::perl6_box_int__PI(
        pir::div__III(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b)))
}
multi infix:<div>(\$a, \$b) {
    pir::perl6_box_int__PI(
        pir::div__III(pir::repr_unbox_int__IP($a.Int), pir::repr_unbox_int__IP($b.Int)))
}

proto infix:<%>(|$) { * }
multi infix:<%>(Int \$a, Int \$b) {
    pir::perl6_box_int__PI(
        pir::mod__III(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b)))
}
multi infix:<%>(Num \$a, Num \$b) {
    pir::perl6_box_num__PN(
        pir::mod__NNN(pir::repr_unbox_num__NP($a), pir::repr_unbox_num__NP($b)))
}
multi infix:<%>(\$a, \$b) {
    $a.Numeric % $b.Numeric
}
multi infix:<%>(Real \$a, Real \$b) {
    $a.Bridge % $b.Bridge
}

proto infix:<~>(|$) { * }
multi infix:<~>(Str \$a, Str \$b) {
    pir::perl6_box_str__PS(
        pir::concat__SSS(pir::repr_unbox_str__SP($a), pir::repr_unbox_str__SP($b)))
}
multi infix:<~>(\$a, \$b) {
    pir::perl6_box_str__PS(
        pir::concat__SSS(pir::repr_unbox_str__SP($a.Str), pir::repr_unbox_str__SP($b.Str)))
}

proto infix:<==>(|$) { * }
multi infix:<==>(Int \$a, Int \$b) {
    pir::perl6_booleanize__PI(
        pir::iseq__III(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b)))
}
multi infix:<==>(Num \$a, Num \$b) {
    pir::perl6_booleanize__PI(
        pir::iseq__INN(pir::repr_unbox_num__NP($a), pir::repr_unbox_num__NP($b)))
}
multi infix:<==>(\$a, \$b) {
    $a.Numeric == $b.Numeric
}
multi infix:<==>(Real \$a, Real \$b) {
    $a.Bridge == $b.Bridge
}

proto infix:<!=>(|$) { * }
multi infix:<!=>(Int \$a, Int \$b) {
    pir::perl6_booleanize__PI(
        pir::isne__III(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b)))
}
multi infix:<!=>(Num \$a, Num \$b) {
    pir::perl6_booleanize__PI(
        pir::isne__INN(pir::repr_unbox_num__NP($a), pir::repr_unbox_num__NP($b)))
}
multi infix:<!=>(\$a, \$b) {
    $a.Numeric != $b.Numeric
}
multi infix:<!=>(Real \$a, Real \$b) {
    $a.Bridge != $b.Bridge
}

proto infix:«<»(|$) { * }
multi infix:«<»(Int \$a, Int \$b) {
    pir::perl6_booleanize__PI(
        pir::islt__III(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b)))
}
multi infix:«<»(Num \$a, Num \$b) {
    pir::perl6_booleanize__PI(
        pir::islt__INN(pir::repr_unbox_num__NP($a), pir::repr_unbox_num__NP($b)))
}
multi infix:«<»(\$a, \$b) {
    $a.Numeric < $b.Numeric
}
multi infix:«<»(Real \$a, Real \$b) {
    $a.Bridge < $b.Bridge
}

proto infix:«<=»(|$) { * }
multi infix:«<=»(Int \$a, Int \$b) {
    pir::perl6_booleanize__PI(
        pir::isle__III(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b)))
}
multi infix:«<=»(Num \$a, Num \$b) {
    pir::perl6_booleanize__PI(
        pir::isle__INN(pir::repr_unbox_num__NP($a), pir::repr_unbox_num__NP($b)))
}
multi infix:«<=»(\$a, \$b) {
    $a.Numeric <= $b.Numeric
}
multi infix:«<=»(Real \$a, Real \$b) {
    $a.Bridge <= $b.Bridge
}

proto infix:«>»(|$) { * }
multi infix:«>»(Int \$a, Int \$b) {
    pir::perl6_booleanize__PI(
        pir::isgt__III(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b)))
}
multi infix:«>»(Num \$a, Num \$b) {
    pir::perl6_booleanize__PI(
        pir::isgt__INN(pir::repr_unbox_num__NP($a), pir::repr_unbox_num__NP($b)))
}
multi infix:«>»(\$a, \$b) {
    $a.Numeric > $b.Numeric
}
multi infix:«>»(Real \$a, Real \$b) {
    $a.Bridge > $b.Bridge
}

proto infix:«>=»(|$) { * }
multi infix:«>=»(Int \$a, Int \$b) {
    pir::perl6_booleanize__PI(
        pir::isge__III(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b)))
}
multi infix:«>=»(Num \$a, Num \$b) {
    pir::perl6_booleanize__PI(
        pir::isge__INN(pir::repr_unbox_num__NP($a), pir::repr_unbox_num__NP($b)))
}
multi infix:«>=»(\$a, \$b) {
    $a.Numeric >= $b.Numeric
}
multi infix:«>=»(Real \$a, Real \$b) {
    $a.Bridge >= $b.Bridge
}

proto infix:<eq>(|$) { * }
multi infix:<eq>(Str \$a, Str \$b) {
    pir::perl6_booleanize__PI(
        pir::iseq__ISS(pir::repr_unbox_str__SP($a), pir::repr_unbox_str__SP($b)))
}
multi infix:<eq>(\$a, \$b) {
    pir::perl6_booleanize__PI(
        pir::iseq__ISS(pir::repr_unbox_str__SP($a.Str), pir::repr_unbox_str__SP($b.Str)))
}

proto infix:<ne>(|$) { * }
multi infix:<ne>(Str \$a, Str \$b) {
    pir::perl6_booleanize__PI(
        pir::isne__ISS(pir::repr_unbox_str__SP($a), pir::repr_unbox_str__SP($b)))
}
multi infix:<ne>(\$a, \$b) {
    pir::perl6_booleanize__PI(
        pir::isne__ISS(pir::repr_unbox_str__SP($a.Str), pir::repr_unbox_str__SP($b.Str)))
}

proto infix:<lt>(|$) { * }
multi infix:<lt>(Str \$a, Str \$b) {
    pir::perl6_booleanize__PI(
        pir::islt__ISS(pir::repr_unbox_str__SP($a), pir::repr_unbox_str__SP($b)))
}
multi infix:<lt>(\$a, \$b) {
    pir::perl6_booleanize__PI(
        pir::islt__ISS(pir::repr_unbox_str__SP($a.Str), pir::repr_unbox_str__SP($b.Str)))
}

proto infix:<le>(|$) { * }
multi infix:<le>(Str \$a, Str \$b) {
    pir::perl6_booleanize__PI(
        pir::isle__ISS(pir::repr_unbox_str__SP($a), pir::repr_unbox_str__SP($b)))
}
multi infix:<le>(\$a, \$b) {
    pir::perl6_booleanize__PI(
        pir::isle__ISS(pir::repr_unbox_str__SP($a.Str), pir::repr_unbox_str__SP($b.Str)))
}

proto infix:<gt>(|$) { * }
multi infix:<gt>(Str \$a, Str \$b) {
    pir::perl6_booleanize__PI(
        pir::isgt__ISS(pir::repr_unbox_str__SP($a), pir::repr_unbox_str__SP($b)))
}
multi infix:<gt>(\$a, \$b) {
    pir::perl6_booleanize__PI(
        pir::isgt__ISS(pir::repr_unbox_str__SP($a.Str), pir::repr_unbox_str__SP($b.Str)))
}

proto infix:<ge>(|$) { * }
multi infix:<ge>(Str \$a, Str \$b) {
    pir::perl6_booleanize__PI(
        pir::isge__ISS(pir::repr_unbox_str__SP($a), pir::repr_unbox_str__SP($b)))
}
multi infix:<ge>(\$a, \$b) {
    pir::perl6_booleanize__PI(
        pir::isge__ISS(pir::repr_unbox_str__SP($a.Str), pir::repr_unbox_str__SP($b.Str)))
}

proto infix:<leg>(|$) { * }
multi infix:<leg>(Str \$a, Str \$b) {
    # TODO: should be Order::{Same, Increase, Decrease}
    $a eq $b ??  0 !!
    $a lt $b ?? -1 !!
                 1;
}
multi infix:<leg>(\$a, \$b) {
    $a.Str leg $b.Str
}

proto infix:<~|>(|$) { * }
multi infix:<~|>(Str \$a, Str \$b) {
    pir::perl6_box_str__PS(pir::bors__SSS(
        pir::repr_unbox_str__SP($a),
        pir::repr_unbox_str__SP($b),
    ));
}
multi infix:<~|>(\$a, \$b) {
    pir::perl6_box_str__PS(pir::bors__SSS(
        pir::repr_unbox_str__SP($a.Str),
        pir::repr_unbox_str__SP($b.Str),
    ));
}

proto infix:<~&>(|$) { * }
multi infix:<~&>(Str \$a, Str \$b) {
    pir::perl6_box_str__PS(pir::bands__SSS(
        pir::repr_unbox_str__SP($a),
        pir::repr_unbox_str__SP($b),
    ));
}
multi infix:<~&>(\$a, \$b) {
    pir::perl6_box_str__PS(pir::bands__SSS(
        pir::repr_unbox_str__SP($a.Str),
        pir::repr_unbox_str__SP($b.Str),
    ));
}

proto infix:<~^>(|$) { * }
multi infix:<~^>(Str \$a, Str \$b) {
    pir::perl6_box_str__PS(pir::bxors__SSS(
        pir::repr_unbox_str__SP($a),
        pir::repr_unbox_str__SP($b),
    ));
}
multi infix:<~^>(\$a, \$b) {
    pir::perl6_box_str__PS(pir::bxors__SSS(
        pir::repr_unbox_str__SP($a.Str),
        pir::repr_unbox_str__SP($b.Str),
    ));
}

proto prefix:<~^>(|$) { * }
multi prefix:<~^>(Str \$a) {
    pir::perl6_box_str__PS(pir::bnots__SS(pir::repr_unbox_str__SP($a)));
}
multi prefix:<~^>(\$a) {
    pir::perl6_box_str__PS(pir::bnots__SS(pir::repr_unbox_str__SP($a.Str)));
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
multi infix:<**>(Int \$a, Int \$b) {
    pir::perl6_box_int__PI(pir::set__IN(pir::pow__NNN(pir::repr_unbox_int__IP($a), pir::repr_unbox_int__IP($b))));
}
multi infix:<**>(\$a, \$b) {
    $a.Numeric ** $b.Numeric
}
multi infix:<**>(Real \$a, Real \$b) {
    $a.Bridge ** $b.Bridge
}

proto infix:<lcm>(|$) { * }
multi infix:<lcm>(Int \$a, Int \$b) {
    pir::perl6_box_int__PI(pir::lcm__III(
            pir::repr_unbox_int__IP($a),
            pir::repr_unbox_int__IP($b)
        ));
}

proto infix:<gcd>(|$) { * }
multi infix:<gcd>(Int \$a, Int \$b) {
    pir::perl6_box_int__PI(pir::gcd__III(
            pir::repr_unbox_int__IP($a),
            pir::repr_unbox_int__IP($b)
        ));
}
