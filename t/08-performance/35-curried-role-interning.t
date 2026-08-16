use nqp;
use Test;

plan 6;

# Type-check caches match on object identity, so a curried role reached
# through role composition must be the same object as the parameterization
# that writing it in source returns. A duplicate curry never hits the cache
# and every type check against it falls back to the slow accepts_type path,
# making dispatch to a routine with a typed array parameter many times
# slower than dispatch to an untyped equivalent. The language itself cannot
# observe the difference, so these tests assert the identity directly.

{
    role Item[::T] { }
    role Box[::T] does Item[T] { }
    ok nqp::eqaddr(
        nqp::decont(Box[Int].^roles(:!transitive)[0]),
        nqp::decont(Item[Int])
    ), 'role instantiated from a generic curry is the interned parameterization';
}

{
    my Int @a;
    my $found = False;
    for @a.WHAT.^role_typecheck_list -> \t {
        $found = True if nqp::eqaddr(nqp::decont(t), nqp::decont(Positional[Int]));
    }
    ok $found, 'typed array typecheck list holds the interned Positional[Int]';
}

{
    my Int %h;
    my $found = False;
    for %h.WHAT.^role_typecheck_list -> \t {
        $found = True if nqp::eqaddr(nqp::decont(t), nqp::decont(Associative[Int]));
    }
    ok $found, 'typed hash typecheck list holds the interned Associative[Int]';
}

{
    my Int @a;
    my $found = False;
    for @a.WHAT.^role_typecheck_list -> \t {
        $found = True if nqp::eqaddr(nqp::decont(t), nqp::decont(Positional));
    }
    ok $found, 'typed array typecheck list also holds the non-parameterized Positional';
}

{
    role Duo[::T, ::U] { }
    role Half[::T] does Duo[Int, T] { }
    ok nqp::eqaddr(
        nqp::decont(Half[Str].^roles(:!transitive)[0]),
        nqp::decont(Duo[Int, Str])
    ), 'curry mixing concrete and generic args instantiates to the interned parameterization';
}

{
    sub a is revision-gated("6.e") { }
    sub b is revision-gated("6.e") { }
    ok nqp::eqaddr(nqp::decont(&a.WHAT), nqp::decont(&b.WHAT)),
        'routines gated on the same revision share their mixin type';
}

# vim: expandtab shiftwidth=4
