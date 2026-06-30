use Test;

plan 5;

# A bare topic variable on the right of a smartmatch refers to the outer topic,
# the same as any other variable. It is not rebound to the left-hand side.

is (given 99 { 14 ~~ $_ }), False,
    'a bare $_ on the smartmatch RHS matches against the outer topic, not the LHS';

is (given 14 { 14 ~~ $_ }), True,
    'a bare $_ on the smartmatch RHS that equals the LHS still matches';

# Reusing the topic on the RHS must not consume the LHS iterator. JSON::Schema
# does `$_.unique ~~ $_` to test a list is unique, which threw X::Seq::Consumed.
is (given <a b a> { .unique ~~ $_ }), False,
    'reusing the topic on the smartmatch RHS does not consume the LHS';
lives-ok { given <a b a> { .unique ~~ $_ } },
    'a Seq LHS against the topic RHS does not throw';

# A block on the RHS still topicalises the LHS.
is (5 ~~ { $_ > 3 }), True,
    'a block on the smartmatch RHS still binds $_ to the LHS';

# vim: expandtab shiftwidth=4
