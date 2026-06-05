use Test;

plan 4;

# `|*.foo` in an argument list is a WhateverCode whose body slips the
# result of `$_.foo`. ArgList must keep the WhateverCode intact instead
# of treating it as a regular `|EXPR` slip-flattening argument and
# unwrapping it to the bare `*.foo` operand.

is-deeply (1, 2, 3).map(|*.list).List, (1, 2, 3),
    '|*.list as a positional arg curries and passes a WhateverCode';

is-deeply ((1, 2, 3).map: |*.list).List, (1, 2, 3),
    '|*.list as a colon-form arg curries and passes a WhateverCode';

is-deeply blob8.new(1, 2, 3, 4).map(|*.polymod(256 xx 3).reverse).List,
    (0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4),
    '|*.polymod(...).reverse curries and slips the polymod result';

# Plain `|EXPR` without a Whatever must still flatten.
sub probe(*@a) { @a.elems }
is probe(0, |(1, 2, 3), 4), 5,
    '|(...) without a Whatever still flattens in the arglist';

# vim: expandtab shiftwidth=4
