use Test;

plan 3;

# The variable case of an enum (`enum $foo <...>`) is not yet implemented and
# must be rejected at compile time rather than silently parsed as a no-op.
throws-like 'enum $foo <a b c>', X::Comp::NYI,
    'enum with a variable name panics NYI';

# A normal named enum is unaffected.
is EVAL('enum Day <Mon Tue Wed>; Day::Wed.value'), 2,
    'a named enum still composes';

# An anonymous enum is unaffected.
is EVAL('(enum <r g b>).<g>'), 1,
    'an anonymous enum still composes';

# vim: expandtab shiftwidth=4
