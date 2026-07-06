use Test;

# A parametric role whose body does a symbolic stash lookup on its own
# type-capture parameter, `T::{$key}`, must compose. Once a class binds `T` to
# a concrete type, `$key` resolves in that type's stash at runtime. RakuAST
# crashed at parse time reaching into the unbound generic's null stash
# (surfaced through Libarchive's BitEnum role); legacy always accepted it.

plan 5;

# The key comes from a variable, looked up in a method.
{
    role Sym1[::T] { method look(Str $k) { T::{$k} } }
    my enum E1 ( Ann => 1, Bob => 2 );
    class C1 does Sym1[E1] {}
    is C1.new.look("Ann"), E1::Ann,
        'T::{$key} in a method resolves in the bound type stash';
    is +C1.new.look("Bob"), 2,
        'the resolved value is the bound type member';
}

# The lookup lives in a lexical sub in the role body, as in BitEnum's `lookup`.
{
    role Sym2[::T] {
        sub find(Str $k) { T::{$k} // die "no such $k" }
        method m(Str $k) { find($k) }
    }
    my enum E2 ( Cat => 10, Dog => 20 );
    class C2 does Sym2[E2] {}
    is +C2.new.m("Dog"), 20,
        'T::{$key} in a lexical sub resolves in the bound type stash';
}

# A literal key resolves the same way.
{
    role Sym3[::T] { method only { T::{"Emu"} } }
    my enum E3 ( Emu => 7 );
    class C3 does Sym3[E3] {}
    is +C3.new.only, 7,
        'T::{"literal"} resolves in the bound type stash';
}

# A key with no matching symbol resolves to an undefined value, not a crash.
{
    role Sym4[::T] { method look(Str $k) { T::{$k} } }
    my enum E4 ( Fox => 1 );
    class C4 does Sym4[E4] {}
    nok C4.new.look("Nope").defined,
        'a missing key resolves to an undefined value';
}

# vim: expandtab shiftwidth=4
