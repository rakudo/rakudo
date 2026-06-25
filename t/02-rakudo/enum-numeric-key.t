use Test;

plan 4;

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

# vim: expandtab shiftwidth=4
