use Test;

plan 3;

# The path only has a value at BEGIN time, as in the common
# `use lib "{$*PROGRAM.dirname}/../lib"` pattern.
{
    use lib "{$*PROGRAM.dirname}/lib";
    pass 'use lib with a block interpolation compiles';
}

use MONKEY-SEE-NO-EVAL;

lives-ok { EVAL 'use lib "x{ 1 + 1 }y"; 1' },
    'use lib with an interpolated expression compiles';

lives-ok { EVAL 'use lib "{ "a" }b", "c{ "d" }"; 1' },
    'use lib with multiple interpolated strings compiles';

# vim: expandtab shiftwidth=4
