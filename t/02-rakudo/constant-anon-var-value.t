use Test;

plan 6;

# A `constant`'s value is computed by a thunk compiled on its own. An
# anonymous variable declared in that value must be declared in the thunk,
# not in the enclosing scope where the thunk cannot reach its storage.

is (constant C1 = (my uint32 $ = -1)), 4294967295,
    'anonymous native unsigned variable initialises the constant';

is (constant C2 = (my int $ = 5)), 5,
    'anonymous native int variable initialises the constant';

is (constant C3 = (my num64 $ = 1.5e0)), 1.5,
    'anonymous native num variable initialises the constant';

is (constant C4 = (my $ = 42)), 42,
    'anonymous object variable initialises the constant';

{
    constant C5 = (my uint32 $ = 7);
    my $after = 3;
    is $after, 3, 'a declaration after the constant is unaffected';
}

{
    sub f($x = (my uint32 $ = 9)) { $x }
    is f(), 9, 'anonymous native variable in a parameter default works';
}
