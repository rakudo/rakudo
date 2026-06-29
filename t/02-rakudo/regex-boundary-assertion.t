use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

plan 3;

# A `<| ... >` boundary assertion names a boundary. `w` is a word boundary and
# `c` a codepoint boundary (always matches at our Unicode level). Any other
# boundary name is unrecognized and matches the empty string rather than failing
# to compile.

is-run q:to/CODE/,
    say so 'ab' ~~ / a <|b> b /;
    CODE
    :out("True\n"),
    'an unrecognized `<|b>` boundary compiles and matches the empty string';

is-run q:to/CODE/,
    say so 'ab'  ~~ / a <|w> b /;
    say so 'a b' ~~ / a <|w> ' ' b /;
    CODE
    :out("False\nTrue\n"),
    '`<|w>` is a word boundary';

is-run q:to/CODE/,
    say so 'ab' ~~ / a <|c> b /;
    CODE
    :out("True\n"),
    '`<|c>` codepoint boundary always matches at our Unicode level';

# vim: expandtab shiftwidth=4
