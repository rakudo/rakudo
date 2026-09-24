use Test;

# The first call of a routine at BEGIN or CHECK time compiles it on
# demand. A native return value of that call must reach the caller boxed
# in the Raku type, as every later call does.

plan 8;

use MONKEY-SEE-NO-EVAL;

is EVAL(q:to/CODE/), 'Int',
        sub f(--> int) { 1 }
        my $r;
        BEGIN $r = f();
        $r.^name
        CODE
    'a BEGIN-time int return stored in a Scalar is an Int';

is EVAL(q:to/CODE/), 'Int',
        sub f(--> int32) { 1 }
        my $r;
        CHECK $r = f();
        $r.^name
        CODE
    'a CHECK-time int32 return stored in a Scalar is an Int';

is EVAL(q:to/CODE/), 'Num',
        sub f(--> num64) { 1e0 }
        my $r;
        BEGIN $r = f();
        $r.^name
        CODE
    'a BEGIN-time num64 return stored in a Scalar is a Num';

is EVAL(q:to/CODE/), 'Str',
        sub f(--> str) { 's' }
        my $r;
        BEGIN $r = f();
        $r.^name
        CODE
    'a BEGIN-time str return stored in a Scalar is a Str';

is EVAL(q:to/CODE/), 1,
        sub f(--> int) { 1 }
        my Int $r;
        BEGIN $r = f();
        $r
        CODE
    'a BEGIN-time int return passes an Int type check';

is EVAL(q:to/CODE/), 'Int',
        sub f(--> int) { 1 }
        BEGIN &f().^name
        CODE
    'a BEGIN-time int return through the code object is an Int';

is EVAL(q:to/CODE/), 'Int',
        multi f(Int $x --> int) { $x }
        my $r;
        BEGIN $r = f(1);
        $r.^name
        CODE
    'a BEGIN-time int return of a multi candidate is an Int';

is EVAL(q:to/CODE/), 'Int',
        sub f(--> int) { 1 }
        constant C = f();
        C.^name
        CODE
    'a constant initialized from an int return is an Int';

# vim: expandtab shiftwidth=4
