use Test;
use MONKEY-SEE-NO-EVAL;

plan 6;

# A role declaration may shadow a type of the same name that comes from the
# setting (e.g. the `Hash::Ordered` module declares `role Hash::Ordered`, which
# shares its name with CORE's `Hash::Ordered` class).
lives-ok { EVAL 'role Hash::Ordered { }' },
    'a role can shadow a setting class of the same compound name';
lives-ok { EVAL 'role Int { method r { 1 } }' },
    'a role can shadow a setting class of the same simple name';

# Shadowing applies to a setting role too: the fresh role replaces the setting
# one rather than being added as a candidate to the setting's role group.
is EVAL('role Numeric { }; so 3.5 ~~ Numeric'), False,
    'a role shadows a setting role of the same name instead of joining it';

# A same-name declaration in the compilation unit is still a redeclaration,
# whether in the same scope or an enclosing one.
throws-like 'class Foo { }; role Foo { }', X::Redeclaration,
    'a role redeclaring a same-scope class is rejected';
throws-like 'class Outer { }; { role Outer { } }', X::Redeclaration,
    'a role redeclaring an enclosing-scope class is rejected';

# Several same-named roles still combine into one role group.
lives-ok { EVAL 'role G { }; role G[::T] { }' },
    'same-named roles still form a role group';

# vim: expandtab shiftwidth=4
