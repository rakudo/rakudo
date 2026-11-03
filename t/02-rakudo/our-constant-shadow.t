use Test;

# A `constant` defaults to `our`. One declared in a nested block shadows
# an outer constant of the same name and overwrites the package entry,
# rather than failing to compile with "already have an 'our constant ...'".

plan 4;

use MONKEY-SEE-NO-EVAL;

is EVAL(q:to/CODE/), 2,
        constant T = 1;
        { constant T = 2; T }
        CODE
    'a nested-block constant shadows the outer one of the same name';

is EVAL(q:to/CODE/), 1,
        constant T = 1;
        { constant T = 2 }
        T
        CODE
    'the outer constant is unchanged lexically outside the block';

is EVAL(q:to/CODE/), 2,
        constant T = 1;
        { constant T = 2 }
        OUR::<T>
        CODE
    'the nested constant overwrites the package entry';

throws-like {
    EVAL q:to/CODE/
        constant T = 1;
        constant T = 2;
        CODE
}, X::Redeclaration, 'a same-scope constant redeclaration still fails';

# vim: expandtab shiftwidth=4
