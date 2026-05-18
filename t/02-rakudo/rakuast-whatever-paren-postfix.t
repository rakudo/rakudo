use Test;

plan 9;

# Parentheses around a postfix operand act as a currying border, so the
# postfix applies to the value of the parenthesized expression rather
# than being absorbed into a surrounding WhateverCode.

# The bug: `(5 ~~ *).WHAT` returned a WhateverCode *instance* (the
# curried thunk for `{ (5 ~~ $_).WHAT }`) instead of the WhateverCode
# *type object*. Bind to a variable first so the surrounding test
# infrastructure does not itself get absorbed into the currying.

{
    my $r = (5 ~~ *).WHAT;
    ok $r === WhateverCode,
        '(5 ~~ *).WHAT returns the WhateverCode type object';
}

{
    my $r = (*+1).WHAT;
    ok $r === WhateverCode,
        '(*+1).WHAT returns the WhateverCode type object';
}

{
    my $r = ((5 ~~ *)).WHAT;
    ok $r === WhateverCode,
        '((5 ~~ *)).WHAT (double parens) still borders currying';
}

# Currying still happens when the postfix is applied directly without
# intervening parens, so `*.foo` and friends are unaffected.

{
    my $r = *.Str;
    isa-ok $r, WhateverCode, '*.Str (no parens) curries to WhateverCode';
    ok $r.defined, '*.Str produced a concrete curried instance';
    is $r(42), '42', '*.Str curried thunk runs with arg';
}

{
    my $r = *.abs;
    isa-ok $r, WhateverCode, '*.abs (no parens) curries to WhateverCode';
    ok $r.defined, '*.abs produced a concrete curried instance';
}

{
    my $r = *+1;
    is $r(10), 11, '*+1 (no parens) still curries and runs';
}

# vim: ft=raku
