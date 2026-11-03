use Test;

plan 4;

# Under `use fatal`, a flattening call argument (|@a or |%h) compiles the
# synthetic FLATTENABLE_LIST/FLATTENABLE_HASH calls with a :flat flag. The
# fatalize pass must not wrap those in FATALIZE, or the flag lands on the
# wrapper and the callsite flattener dies on a non-array.

lives-ok { use fatal; sub f(:%h) { 42 }; my %c; f(|%c) },
    'use fatal: flattening an empty hash argument';

lives-ok { use fatal; sub f(*@a) { @a.elems }; my @x = 1, 2, 3; f(|@x) },
    'use fatal: flattening an array argument';

lives-ok { use fatal; sub f(*@a, *%h) { "{@a.elems}/{%h.elems}" };
           my @x = 1, 2; my %h = :z(9); f(|@x, |%h) },
    'use fatal: flattening an array and a hash argument together';

# The operand is still fatalized, so a Failure flowing into the flatten throws.
dies-ok { use fatal; sub bad() { fail "boom" }; sub g(*@a) { 1 }; g(|bad()) },
    'use fatal: a Failure flattened into a call still throws';

# vim: expandtab shiftwidth=4
