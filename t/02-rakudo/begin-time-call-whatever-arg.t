use Test;

# A trait applied with a WhateverCode argument (e.g. `is foo(*.flip)`) is
# compiled and evaluated at BEGIN, so the trait receives a working Callable
# rather than failing with "BEGIN time calls only supported for simple
# constructs". Each case applies the WhateverCode inside the trait at BEGIN
# and records the result.

plan 3;

use MONKEY-SEE-NO-EVAL;

is EVAL(q:to/CODE/), 'olleh',
        my @log;
        multi sub trait_mod:<is>(Parameter $p, :&conv!) { @log.push(conv("hello")) }
        sub f($x is conv(*.flip)) {}
        @log[0]
        CODE
    'a WhateverCode argument to a parameter trait reaches it as a Callable';

is EVAL(q:to/CODE/), 11,
        my @log;
        multi sub trait_mod:<is>(Routine $r, :&conv!) { @log.push(conv(10)) }
        sub f() is conv(* + 1) {}
        @log[0]
        CODE
    'a WhateverCode argument to a routine trait reaches it as a Callable';

is EVAL(q:to/CODE/), 'ABC',
        my @log;
        multi sub trait_mod:<is>(Variable $v, :&conv!) { @log.push(conv("abc")) }
        my $x is conv(*.uc);
        @log[0]
        CODE
    'a WhateverCode argument to a variable trait reaches it as a Callable';

# vim: expandtab shiftwidth=4
