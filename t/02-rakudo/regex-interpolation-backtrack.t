use Test;

plan 8;

# An array interpolated into a regex is an alternation of its elements, so
# when the rest of the regex fails after one element matched, the regex
# backtracks into the interpolation and tries the next element, the same
# way it does for the literal `< a ab >` form.

my @alts = < a ab >;

is 'abc' ~~ / @alts b /, 'ab',
    'an interpolated array backtracks to a shorter element';

is 'abc' ~~ / @(< a ab >) b /, 'ab',
    'an interpolated list backtracks to a shorter element';

is 'abc' ~~ / <@alts> b /, 'ab',
    'an array assertion backtracks to a shorter element';

is 'abc' ~~ / ||@alts c /, 'abc',
    'a sequential interpolated array backtracks to a later element';

is 'abc' ~~ / @alts /, 'ab',
    'an interpolated array prefers the longest element';

is ('abc' ~~ m:ex/ @alts b? /).join('|'), 'ab|ab|a',
    'exhaustive matching backtracks into an interpolated array after each match';

my @rx = (rx/a+/, rx/aab/);
is 'aabx' ~~ / [||@rx] x /, 'aabx',
    'an exhausted regex element gives way to the next element';

my token alts { @alts b }
nok 'abc' ~~ /<alts>/,
    'a token does not backtrack into an interpolated array';

# vim: expandtab shiftwidth=4
