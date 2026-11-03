use Test;

plan 6;

# A sub-signature forces an `@` sigil on its parameter, but unlike a real array
# variable the leading type does not parameterise the bound role. The bound type
# stays a bare `Positional` so a plain Array still binds and destructures.

sub typed-subsig(Int [$a, $b]) { "$a $b" }
is typed-subsig([1, 2]), '1 2',
    'a typed anonymous sub-signature binds and destructures a plain Array';

sub untyped-subsig([$a, $b]) { "$a $b" }
is untyped-subsig([3, 4]), '3 4',
    'an untyped anonymous sub-signature still binds a plain Array';

is &typed-subsig.signature.params[0].type.^name, 'Positional',
    'a typed sub-signature parameter is a bare Positional, not Positional[Int]';

# A real `@` variable keeps parameterising the role by its element type.
sub real-array(Int @a) { @a.elems }
is &real-array.signature.params[0].type.^name, 'Positional[Int]',
    'a real typed array parameter still parameterises Positional by its element type';

my Int @ints = 1, 2, 3;
is real-array(@ints), 3,
    'a real typed array parameter still binds a matching Positional';

# A real `@` variable that also carries a sub-signature keeps its element type.
sub array-with-subsig(Int @a [$a, $b]) { @a.elems }
is &array-with-subsig.signature.params[0].type.^name, 'Positional[Int]',
    'a real typed array with a sub-signature keeps Positional[Int]';

# vim: expandtab shiftwidth=4
