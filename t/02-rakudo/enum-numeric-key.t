use Test;

plan 17;

# A numeric-literal key reaches enum building as an Int, not a Str.
is EVAL('my enum E (A => 1, 12844 => 25); E.enums<12844>'), 25,
    'enum value with a numeric-literal key is reachable by its string key';

# A sunk declaration must not run ENUM_VALUES, which needs string keys.
lives-ok { EVAL 'my enum E (A => 1, 12844 => 25); 0' },
    'a sunk enum declaration with a numeric-literal key composes';

is EVAL('my $t = do { my enum E (A => 1, 12844 => 25); E }; $t.enums<A>'), 1,
    'enum declared then discarded inside a do block composes';

is EVAL('enum Day <Mon Tue Wed>; Day::Wed.value'), 2,
    'an enum with identifier keys is unaffected';

is-deeply EVAL('my enum E (5.succ); E.enums'), Map.new((6 => 0)),
    'a bare key computed as an Int becomes a key counting from 0';

is-deeply EVAL('my enum E (6); E.enums'), Map.new((6 => 0)),
    'a bare Int key becomes a key counting from 0';

is-deeply EVAL('my enum E (1.5, 2); E.enums'), Map.new((1.5 => 0, 2 => 1)),
    'bare numeric keys of different types count from 0';

is-deeply EVAL('my enum E (6, b => 5); E.enums'), Map.new((6 => 0, b => 5)),
    'a bare Int key before a pair counts from 0';

is-deeply EVAL('my enum E (a => 5, 6); E.enums'), Map.new((a => 5, 6 => 6)),
    'a bare Int key after a pair counts on from the value of the pair';

is-deeply EVAL(q|my enum E ('a', 3, 'b'); E.enums|), Map.new((a => 0, 3 => 1, b => 2)),
    'bare Str and Int keys share one count';

is-deeply EVAL('my enum A <x y>; { my enum B (A::x, A::y); B.enums }'), Map.new((x => 0, y => 1)),
    'values of another enum become keys named by their Str';

is-deeply EVAL('my $m = (my enum E (6, 7)); $m'), Map.new((6 => 0, 7 => 1)),
    'an enum declaration with bare Int keys evaluates to a Map of its keys';

is-deeply EVAL('my $m = (my enum E (6 => 1)); $m'), Map.new((6 => 1)),
    'an enum declaration with an Int pair key evaluates to a Map of its keys';

lives-ok { EVAL 'my module M { my enum E (6) }' },
    'an enum with a bare Int key that ends a package body composes';

is-deeply EVAL('my enum N (a => <1>, b => <2>); my $m = (my enum E (N::a, N::b)); $m'),
    Map.new((1 => 0, 2 => 1)),
    'an enum declaration keyed by values of an allomorph enum evaluates to a Map of its keys';

is-deeply EVAL('my $m = (my enum E (<a b>, <c d>)); $m'), Map.new(("a b" => 0, "c d" => 1)),
    'an enum declaration with list items evaluates to a Map with a key for each item';

is-deeply EVAL('my $x; BEGIN $x = <a b>; my $m = (my enum E ($x)); $m'), Map.new((a => 0, b => 1)),
    'an enum declaration of an itemized list evaluates to a Map with a key for each element';

# vim: expandtab shiftwidth=4
