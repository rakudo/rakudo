use Test;

plan 6;

# A `constant` initialized from a statement prefix such as `gather` is evaluated
# at compile time. The value is the result of running the prefix, not the block
# it wraps, so a `%`/`@`/`$` constant gets the gathered values.

my constant %RULES = gather { take 'a' => 1; take 'b' => 2 };
is %RULES<a>, 1, 'a %-constant from gather holds the gathered pairs';
is %RULES.elems, 2, 'a %-constant from gather has all the pairs';

my constant @LIST = gather { take 1; take 2; take 3 };
is @LIST.join(','), '1,2,3', 'an @-constant from gather holds the gathered values';

my constant $SEQ = gather { take 42 };
is $SEQ.List.join(','), '42', 'a $-constant from gather holds the gathered sequence';

# A `constant` initialized from a block or routine literal still binds the code
# object itself, not the result of running it.
my constant &BLOCK = { 99 };
is BLOCK(), 99, 'a &-constant from a block still binds the block';

my constant $PROMISE = start { 7 };
is $PROMISE.result, 7, 'a $-constant from start runs and holds the promise';

# vim: expandtab shiftwidth=4
