use Test;

plan 10;

# `$x but Role<value>`, `$x does Role<value>` and `$x but Role(value)`
# hand the role and a named value to the mixin operator, so a role with
# one public attribute has it initialized from the value. Any other call
# on the right hand side mixes in its result.

my role Typed { has $.type }

{
    my $x = 42 but Typed<and>;
    is $x.type, 'and', 'but Role<value> initializes the attribute';
    isa-ok $x, Int, 'but Role<value> keeps the original type';
}

{
    my $x = 43;
    $x does Typed<and>;
    is $x.type, 'and', 'does Role<value> initializes the attribute';
}

{
    my $x = 44 but Typed<<and>>;
    is $x.type, 'and', 'but Role<<value>> initializes the attribute';
}

{
    my $x = 45 but Typed<a b>;
    is-deeply $x.type, $("a", "b"),
        'but Role<a b> initializes the attribute with the list';
}

{
    my $x = 46 but Typed("and");
    is $x.type, 'and', 'but Role(value) initializes the attribute';
}

{
    my $s = "b";
    is (47 but "a" ~ $s).Str, "ab",
        'but with a concatenation on the right mixes in its result';
    is (48 but 1 + 2 * $s.chars).Int, 3,
        'but with an addition on the right mixes in its result';
}

{
    sub two($a, $b) { Typed }
    my $x = 49 but two(Typed, "x");
    is-deeply $x.type, Any,
        'but with a two argument sub call on the right calls the sub';
}

{
    my $r = Typed;
    dies-ok { 50 but $r<and> },
        'but with an index on a variable is not taken apart';
}

# vim: expandtab shiftwidth=4
