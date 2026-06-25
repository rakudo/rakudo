use Test;

plan 4;

# `is ::Foo` inherits from the existing type Foo, not a fresh type capture,
# even when the enclosing package shares the name.

is do {
    class O1 { role Exception is ::Exception { method m { 42 } } }
    class I1 does O1::Exception { }
    I1.new.m
}, 42, 'role inheriting from a same-named outer type via ::Name composes';

ok do {
    class O2 { role Exception is ::Exception { } }
    class I2 does O2::Exception { }
    I2 ~~ Exception
}, 'the consuming class is-a the outer type';

is do {
    class C is ::Int { }
    C.^mro.elems
}, 5, 'a class inheriting an existing type via ::Name has its real MRO';

# A genuine type capture in a signature is unaffected: it binds to the
# argument type rather than resolving to an existing same-named type.
is do {
    sub f(::Int $x) { Int.^name }
    f(3.5)
}, 'Rat', 'a signature ::Name capture still binds to the argument type';

# vim: expandtab shiftwidth=4
