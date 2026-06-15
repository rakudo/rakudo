use Test;

plan 10;

# A type object with a user-defined ACCEPTS runs it for a smartmatch rather than
# reducing to a type check (rakudo #5762).
{
    my $ran = 0;
    my class C { method ACCEPTS($) { ++$ran; True } }
    ok 42 ~~ C, 'a type object with a user ACCEPTS runs it for a smartmatch';
    is $ran, 1, 'the user ACCEPTS ran once';
}

# An ACCEPTS mixed in by a role onto a built-in type is user-defined too, the
# shape Needle::Compile's StrType relies on.
{
    my $ran = 0;
    my role R { method ACCEPTS($) { ++$ran; True } }
    my constant T = Int but R;
    ok 42 ~~ T, 'a role-supplied ACCEPTS on a but-mixed type object is called';
    is $ran, 1, 'the role ACCEPTS ran once';
}

# A candidate constrained to a defined invocant cannot run for a type object, so
# the type check applies and that ACCEPTS is not called.
{
    my $ran = 0;
    my class D { multi method ACCEPTS(D:D: $) { ++$ran; True } }
    nok 42 ~~ D, 'a :D-only ACCEPTS does not run for a type-object right side';
    is $ran, 0, 'so the type check applies and the ACCEPTS is not called';
}

# Negation runs the same ACCEPTS and inverts its result.
{
    my $ran = 0;
    my class N { method ACCEPTS($) { ++$ran; False } }
    ok 42 !~~ N, 'a negated smartmatch runs the user ACCEPTS and inverts it';
    is $ran, 1, 'the user ACCEPTS ran once for the negated form';
}

# A type whose ACCEPTS comes from the setting still reduces to a type check.
ok  42  ~~ Int, 'a setting type still type checks, a match';
nok "x" ~~ Int, 'a setting type still type checks, a non-match';

# vim: expandtab shiftwidth=4
