use Test;

plan 7;

# A `constant` is evaluated at BEGIN. Referencing a non-constant outer lexical
# in its initializer (here a sigilless `my \elements`, whose binding runs later
# at mainline time) must not die with "Symbol 'elements' does not have a
# compile-time value". The lexical is unbound at that BEGIN, so it resolves to
# Mu, the same as the legacy frontend, which refers to it in place.
my \elements = 1, 2, 3;
constant WithNonConstant = [elements];
is WithNonConstant.elems, 1,
    'a constant referencing a non-constant my-\ lexical compiles';
nok WithNonConstant[0].defined,
    'the unbound lexical resolves to its undefined value at the constant BEGIN';

# Consumed as a bare value rather than inside a list, the constant must hold a
# real Mu, not a raw VMNull (which would have no methods).
my \bare = 42;
constant Bare = bare;
nok Bare.defined,
    'a non-constant sigilless lexical yields Mu when held as the bare value';

# A sigil'd variable has a container, so it yields that container default at
# BEGIN: Any for a scalar, an empty Array for a positional.
my $scalar;
constant FromScalar = $scalar;
nok FromScalar.defined, 'a non-constant scalar lexical yields its Any default';
my @positional;
constant FromArray = @positional;
is FromArray.elems, 0, 'a non-constant array lexical yields its empty default';

# A constant referencing a real constant still inlines its value, including a
# constant resolved from an enclosing (EVAL) scope.
constant Base = 5;
constant Derived = Base * 2;
is Derived, 10, 'a constant referencing a constant still binds its value';
is EVAL(q:to/CODE/), 10, 'a constant referencing an outer-scope constant inlines it';
    constant Inner = Base * 2;
    Inner
    CODE

# vim: expandtab shiftwidth=4
