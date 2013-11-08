## Order enumeration, for cmp and <=>
my enum Order (:Less(-1), :Same(0), :More(1));

sub Increase { DEPRECATED("Less"); Less }
sub Decrease { DEPRECATED("More"); More }

proto infix:<cmp>($, $) { * }
multi infix:<cmp>(\a, \b) {
    return Order::Less if a === -$Inf || b === $Inf;
    return Order::More if a ===  $Inf || b === -$Inf;
    a.Stringy cmp b.Stringy
}
multi infix:<cmp>(Real \a, Real \b) { a.Bridge cmp b.Bridge }
multi infix:<cmp>(Int:D \a, Int:D \b) {
    Order.(nqp::p6box_i(nqp::cmp_I(nqp::decont(a), nqp::decont(b))))
}
multi infix:<cmp>(int $a, int $b) {
    Order.(nqp::p6box_i(nqp::cmp_i($a, $b)))
}

multi infix:«<=>»(Int:D \a, Int:D \b) {
    Order.(nqp::p6box_i(nqp::cmp_I(nqp::decont(a), nqp::decont(b))))
}
multi infix:«<=>»(int $a, int $b) {
    Order.(nqp::p6box_i(nqp::cmp_i($a, $b)))
}


