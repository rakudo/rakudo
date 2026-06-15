use Test;

plan 7;

# A type object whose ACCEPTS comes from the setting, even one mixed in from a
# role, makes a smartmatch a type check rather than an ACCEPTS call.

my $ran = 0;
my role R { method ACCEPTS($) { ++$ran; True } }

{
    my $T := Str but R;
    my $v := "x" but R;
    ok $v ~~ $T, 'a mixed-in type object in a variable does a type check';
    nok 42 ~~ $T, 'a value of the wrong type is rejected by the type check';
}

{
    my constant T = Str but R;
    my $v := "y" but R;
    ok $v ~~ T, 'a mixed-in type object in a constant does a type check';
    ok ("z" but R) ~~ (Str but R),
        'a mixed-in type object from an expression does a type check';
}

{
    my $T := Str but R;
    ok 42 !~~ $T, 'negated smartmatch reduces to a type check too';
}

is $ran, 0, 'the mixed-in ACCEPTS was never called';

# A type object whose ACCEPTS the setting cannot vouch for is still called.
my role S { method ACCEPTS($) { "matched" } }
ok "a" ~~ S.new, 'a concrete matcher still runs its own ACCEPTS';

# vim: expandtab shiftwidth=4
