use Test;

plan 9;

# `(5 ~~ *).WHAT` returned a curried WhateverCode whose deparsed string
# was `.WHAT` (and similarly for `.HOW`/`.WHO`/`.VAR`/etc.). Those
# special-op postfixes compile to primitive nqp:: ops, not real method
# calls, so they sit outside the currying machinery: `(curried).WHAT`
# must evaluate the parens to a WhateverCode and then take its WHAT.

# The discriminating check is type-object identity, not isa-ok: in the
# bug state `(5 ~~ *).WHAT` was a WhateverCode *instance*, not the
# WhateverCode *type object*. Bind to a variable first so the
# surrounding test machinery does not itself get absorbed into currying.

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

# Non-special-op postfixes (regular method calls) MUST still curry
# through the parens, since that is how legacy and the wider ecosystem
# build expressions like `("- " ~ *).chars` and use them with `.map`.

{
    my $c = ("- " ~ *).chars;
    isa-ok $c, WhateverCode, '("- " ~ *).chars produces a WhateverCode';
    ok $c.defined, '... a concrete curried instance';
    is $c("hi"), 4, '... that runs ("- " ~ $_).chars';
}

# A bare `*` directly under a postfix (no parens) still curries.

{
    my $r = *.Str;
    isa-ok $r, WhateverCode, '*.Str (no parens) curries to WhateverCode';
    ok $r.defined, '*.Str produces a concrete curried instance';
    is $r(42), '42', '*.Str curried thunk runs with arg';
}

{
    my $r = *+1;
    is $r(10), 11, '*+1 (no parens) still curries and runs';
}

# vim: ft=raku
