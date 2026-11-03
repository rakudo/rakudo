use Test;

plan 2;

# A module defines its export tags by declaring its own `my package EXPORT`
# and populating it (here via OUR::<...>). That declaration shares a name with
# the compunit's implicit EXPORT lexical, so it has to replace that implicit
# rather than collide with it.
my package EXPORT {
    OUR::<MARKER> := 12345;
}

is EXPORT::<MARKER>, 12345, 'the declared EXPORT package carries its OUR:: symbols';
is EXPORT.^name, 'EXPORT', 'EXPORT resolves to the user-declared package';
