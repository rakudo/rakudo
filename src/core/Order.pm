## Order enumeration, for cmp and <=>
my enum Order (:Increase(-1), :Same(0), :Decrease(1));

proto infix:<cmp>($, $) { * }
multi infix:<cmp>(\a, \b) {
    return Order::Increase if a === -$Inf || b === $Inf;
    return Order::Decrease if a ===  $Inf || b === -$Inf;
    a.Stringy cmp b.Stringy
}
multi infix:<cmp>(Real \a, Real \b) { a.Bridge cmp b.Bridge }
multi infix:<cmp>(Int:D \a, Int:D \b) {
    Order.(nqp::p6box_i(nqp::cmp_I(nqp::p6decont(a), nqp::p6decont(b))))
}
multi infix:<cmp>(int $a, int $b) {
    Order.(nqp::p6box_i(nqp::cmp_i($a, $b)))
}

multi infix:«<=>»(Int:D \a, Int:D \b) {
    Order.(nqp::p6box_i(nqp::cmp_I(nqp::p6decont(a), nqp::p6decont(b))))
}
multi infix:«<=>»(int $a, int $b) {
    Order.(nqp::p6box_i(nqp::cmp_i($a, $b)))
}


