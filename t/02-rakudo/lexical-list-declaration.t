use nqp;
use Test;

# my (...) without a binding initializer is not destructuring. It
# declares a list of variables followed by list assignment. The
# parenthesized form parses as a signature, so per-variable traits and
# defaults arrive as parameter metadata, and the compiler moves them
# onto the variable declarations. A `:=` initializer or the signature
# literal form `my :(...)` keeps full signature semantics. The legacy
# frontend drops the parameter metadata silently, so those cases are
# skipped there.

my $rakuast := nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';

plan 13;

{
    my ($x, $y) = 1, 2;
    is-deeply ($x, $y), (1, 2),
        'list assignment to a my list still distributes values';
}

{
    my ($a, $b = 5) := \(1);
    is $b, 5,
        'a := initializer keeps parameter defaults as binding semantics';
}

{
    my (:$x, :$y) := (x => 3, y => 4).Capture;
    is-deeply ($x, $y), (3, 4),
        'a := initializer keeps named parameters as binding semantics';
}

if $rakuast {
    is EVAL('my ($a = 5); $a'), 5,
        'a default inside a my list initializes its variable';

    is-deeply EVAL('my ($a = 5, $b = 6)'), (5, 6),
        'a my list used as an rvalue evaluates its defaults';

    is EVAL('my ($a is default(42), $b) = Nil, 1; $a'), 42,
        'a trait inside a my list is applied to its variable';

    throws-like 'my ($a is totally-made-up)',
        X::Comp::Trait::Unknown,
        message => /'variable declaration'/,
        'an unknown trait inside a my list is a compile time error';

    is-deeply EVAL('my ($a = 1, $b) = 9; ($a, $b)'), (9, Any),
        'a variable with a default may precede one without in a my list';

    is EVAL('my ($a = 5) = (); $a.raku'), 'Any',
        'list assignment overwrites a default inside a my list';

    is EVAL('sub f() { state ($n = 10); $n++ }; f; f; f'), 12,
        'a default inside a state list initializes only on the first entry';

    is EVAL('our ($v = 7); $v'), 7,
        'a default inside an our list initializes its variable';

    is EVAL('my Int ($a = 5); $a'), 5,
        'a list level type and a default combine in a my list';

    throws-like { EVAL 'my Str ($c = 1)' },
        X::TypeCheck::Assignment,
        'a default inside a my list is checked against the list level type';
}
else {
    skip 'my list variable traits and defaults are NYI on the legacy frontend', 10;
}

# vim: expandtab shiftwidth=4
