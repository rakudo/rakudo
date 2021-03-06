## Order enumeration, for cmp and <=>
my enum Order (:Less(-1), :Same(0), :More(1));
role Rational { ... }

sub ORDER(int $i --> Order) is implementation-detail {
    $i
      ?? nqp::islt_i($i,0)
        ?? Less
        !! More
      !! Same
}

proto sub infix:<cmp>($, $, *% --> Order:D) is pure {*}
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
    nqp::istype(a,Rational) && nqp::istype(b,Rational)
      ?? a.isNaN || b.isNaN
        ?? a.Num cmp b.Num
        !! a <=> b
      !! (nqp::istype(a, Rational) && nqp::isfalse(a.denominator))
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

multi sub infix:<cmp>(Code:D \a, Code:D \b) {
     a.name cmp b.name
}
multi sub infix:<cmp>(Code:D \a, \b) {
     a.name cmp b.Stringy
}
multi sub infix:<cmp>(\a, Code:D \b) {
     a.Stringy cmp b.name
}

proto sub infix:«<=>»($, $, *% --> Order:D) is pure {*}
multi sub infix:«<=>»(\a, \b)  { a.Real <=> b.Real }
multi sub infix:«<=>»(Real \a, Real \b) { a.Bridge <=> b.Bridge }

multi sub infix:«<=>»(Int:D \a, Int:D \b) {
    ORDER(nqp::cmp_I(nqp::decont(a), nqp::decont(b)))
}
multi sub infix:«<=>»(int $a, int $b) {
    ORDER(nqp::cmp_i($a, $b))
}

proto sub infix:<before>($?, $?, *% --> Bool:D) is pure {*}
multi sub infix:<before>($? --> True) { }
multi sub infix:<before>(\a, \b) {
    nqp::hllbool(nqp::eqaddr((a cmp b),Order::Less))
}

proto sub infix:<after>($?, $?, *% --> Bool:D) is pure {*}
multi sub infix:<after>($x? --> True) { }
multi sub infix:<after>(\a, \b) {
    nqp::hllbool(nqp::eqaddr((a cmp b),Order::More))
}

proto sub infix:<leg>($, $, *% --> Order:D) is pure {*}
multi sub infix:<leg>(\a, \b) { a.Stringy cmp b.Stringy }

proto sub infix:<unicmp>($, $, *% --> Order:D) is pure {*}

# NOT is pure because of $*COLLATION
proto sub infix:<coll>(  $, $, *% --> Order:D) {*}

# vim: expandtab shiftwidth=4
