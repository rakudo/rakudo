use Test;

plan 16;

# A colonpair with a smiley key still takes a value outside a type name. The
# pairs are bound to variables first so they stay positional rather than
# becoming named arguments to is-deeply.
my $d = :D(1);
is-deeply $d, (D => 1), ':D(1) is the pair D => 1';
my $u = :U(2);
is-deeply $u, (U => 2), ':U(2) is the pair U => 2';
my $underscore = :_(3);
is-deeply $underscore, (_ => 3), ':_(3) is the pair _ => 3';

my $b = :D[4, 5];
is-deeply $b, (D => [4, 5]), ':D[...] takes its value';

my @list = :A(1), :B(1), :C(1), :D(1);
is-deeply @list, [A => 1, B => 1, C => 1, D => 1],
    'a smiley-keyed pair in a list keeps its value';

my $t = :D;
is-deeply $t, (D => True), 'valueless :D is D => True';

# The smiley keeps its definedness meaning inside a type name.
ok 5 ~~ Int:D, 'Int:D matches a defined Int';
nok Int ~~ Int:D, 'Int:D does not match an undefined Int';
ok Int ~~ Int:U, 'Int:U matches an undefined Int';
my Int:D $x = 5;
is $x, 5, 'Int:D constraint on a variable works';
lives-ok { sub f(Int:D $a) { $a }; f(7) }, 'Int:D parameter constraint works';

# After a type name the `(...)` is a coercion target, not a colonpair value.
sub g(Str:D(Numeric) $a) { $a }
is g(pi), pi.Str, 'Str:D(Numeric) coerces a Num to a defined Str';
my Str:U(Numeric) $coerced-undef;
ok $coerced-undef ~~ Str:U, 'Str:U(Numeric) keeps its undefinedness smiley';
my $val = Str:D(Numeric).^name;
is $val, 'Str:D(Numeric)', 'Str:D(Numeric) used as a value is the coercion type';

# The standalone-pair handling must not leak into a coercion type nested in a
# colonpair value.
my $nested = :foo(Str:D(Numeric));
is $nested.value.^name, 'Str:D(Numeric)',
    'a coercion type as a colonpair value keeps its coercion';
sub h(*%h) { %h }
is h(:bar(Str:D(Numeric)))<bar>.^name, 'Str:D(Numeric)',
    'a coercion type as a named-argument value keeps its coercion';

# vim: expandtab shiftwidth=4
