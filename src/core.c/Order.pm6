## Order enumeration, for cmp and <=>
my enum Order (:Less(-1), :Same(0), :More(1));
role Rational { ... }

sub ORDER(int $i --> Order) {
    nqp::if($i,nqp::if(nqp::islt_i($i,0),Less,More),Same)
}

proto sub infix:<cmp>($, $, *%) is pure {*}
multi sub infix:<cmp>(\a, \b) {
    nqp::eqaddr(nqp::decont(a), nqp::decont(b))
      ?? Same
      !! a.Stringy cmp b.Stringy
}
multi sub infix:<cmp>(Real:D \a, \b) {
     a === -Inf
       ?? Less
       !! a === Inf
         ?? More
         !! a.Stringy cmp b.Stringy
}
multi sub infix:<cmp>(\a, Real:D \b) {
     b === Inf
       ?? Less
       !! b === -Inf
         ?? More
         !! a.Stringy cmp b.Stringy
}

multi sub infix:<cmp>(Real:D \a, Real:D \b) {
       (nqp::istype(a, Rational) && nqp::isfalse(a.denominator))
    || (nqp::istype(b, Rational) && nqp::isfalse(b.denominator))
    ?? a.Bridge cmp b.Bridge
    !! a === -Inf || b === Inf
        ?? Less
        !! a === Inf || b === -Inf
            ?? More
            !! a.Bridge cmp b.Bridge
}
multi sub infix:<cmp>(Int:D \a, Rational:D \b) {
    a.isNaN || b.isNaN ?? a.Num cmp b.Num !! a <=> b
}
multi sub infix:<cmp>(Rational:D \a, Int:D \b) {
    a.isNaN || b.isNaN ?? a.Num cmp b.Num !! a <=> b
}
multi sub infix:<cmp>(Int:D \a, Int:D \b) {
    ORDER(nqp::cmp_I(nqp::decont(a), nqp::decont(b)))
}
multi sub infix:<cmp>(int $a, int $b) {
    ORDER(nqp::cmp_i($a, $b))
}

multi sub infix:«<=>»(Int:D \a, Int:D \b) {
    ORDER(nqp::cmp_I(nqp::decont(a), nqp::decont(b)))
}
multi sub infix:«<=>»(int $a, int $b) {
    ORDER(nqp::cmp_i($a, $b))
}

# vim: ft=perl6 expandtab sw=4
