## Order enumeration, for cmp and <=>
my enum Order (:Less(-1), :Same(0), :More(1));

sub ORDER(int $i) {
    $i == 0 ?? Same !! $i <  0 ?? Less !! More
}

#?if parrot
proto infix:<cmp>($, $) { * }
#?endif
#?if !parrot
proto infix:<cmp>(Mu $, Mu $) { * }
#?endif
multi infix:<cmp>(\a, \b) {
    return Order::Less if a === -Inf || b === Inf;
    return Order::More if a ===  Inf || b === -Inf;
    a.Stringy cmp b.Stringy
}
multi infix:<cmp>(Real \a, Real \b) { a.Bridge cmp b.Bridge }
multi infix:<cmp>(Int:D \a, Int:D \b) {
    ORDER(nqp::cmp_I(nqp::decont(a), nqp::decont(b)))
}
multi infix:<cmp>(int $a, int $b) {
    ORDER(nqp::cmp_i($a, $b))
}

multi infix:«<=>»(Int:D \a, Int:D \b) {
    ORDER(nqp::cmp_I(nqp::decont(a), nqp::decont(b)))
}
multi infix:«<=>»(int $a, int $b) {
    ORDER(nqp::cmp_i($a, $b))
}

# vim: ft=perl6 expandtab sw=4
