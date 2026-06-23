use Test;

plan 1;

is EVAL('FOO: { 21 * 2 }'), 42,
    'a labeled block with an unreferenced label compiles and yields its value';

# vim: expandtab shiftwidth=4
