use Test;

plan 12;

# A short-circuiting operator thunks an operand, so a bare Whatever standing
# there is a literal Whatever term for the operator to act on, not a point to
# curry a WhateverCode over.

is (0 // *), 0, 'a defined left side of // is returned, not curried';
is (Any // *), *, 'an undefined left side of // yields the literal Whatever';
is (0 orelse *), 0, 'orelse returns its defined left side';
is (0 and *), 0, 'and returns its false left side';
is (5 or *), 5, 'or returns its true left side';
isa-ok (0 // *), Int, 'a short-circuit result is a plain value, not a WhateverCode';

# The reduction that motivated this: an index of 0 through // must stay 0.
{
    my @a = 10, 20, 30;
    my $idx = @a.first(:k, * >= 10);
    is @a.skip($idx // *).elems, 3, 'skip(0 // *) keeps every element';
}

# Non-short-circuiting operators still curry as before.
{
    my $wc = 5 + *;
    isa-ok $wc, WhateverCode, 'a Whatever under + still curries';
    is $wc(3), 8, 'the curried WhateverCode applies';
}
{
    my @a = 1, 2, 3;
    is @a[* - 1], 3, 'a Whatever in a subscript still curries';
}
is (* ~~ 3).WHAT, WhateverCode, 'a Whatever under ~~ still curries';

# Only a bare Whatever is spared. A WhateverCode operand still curries, so
# joining two of them with a short-circuit operator inside a block makes a
# WhateverCode where a block already is one.
throws-like ｢sub ($a where {* < 5 and * > 9}) { }｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a WhateverCode operand of a short-circuit operator still curries';
