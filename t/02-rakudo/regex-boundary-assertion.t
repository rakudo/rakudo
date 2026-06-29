use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

plan 8;

# `<| ... >` names a regex boundary; only `<|w>` (word) and `<|c>` (codepoint)
# are defined. An unknown name no-ops before 6.e and is an error from 6.e on.

is-run q:to/CODE/,
    say so 'ab' ~~ / a <|b> b /;
    CODE
    :out("True\n"),
    'an unrecognized `<|b>` boundary matches the empty string before 6.e';

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

is-run q:to/CODE/,
    use v6.e.PREVIEW;
    'ab' ~~ / a <|b> b /;
    CODE
    :err(/"Unrecognized regex boundary '<|b>'"/),
    :exitcode(1),
    'an unrecognized boundary is a compile error from 6.e';

is-run q:to/CODE/,
    use v6.e.PREVIEW;
    say so 'ab' ~~ / a <|w> b /;
    CODE
    :out("False\n"),
    '`<|w>` is still a valid boundary under 6.e';

is-run q:to/CODE/,
    use v6.e.PREVIEW;
    say so 'ab' ~~ / a <|c> b /;
    CODE
    :out("True\n"),
    '`<|c>` is still a valid boundary under 6.e';

is-run q:to/CODE/,
    use v6.e.PREVIEW;
    'ab' ~~ / a <|zz> b /;
    CODE
    :err(/"Unrecognized regex boundary '<|zz>'"/),
    :exitcode(1),
    'a multi-character unrecognized boundary is reported with its full name';

# The no-op must also hold away from the top of the pattern.
is-run q:to/CODE/,
    say so 'ax' ~~ / a [ <|b> 'x' | 'y' ] /;
    CODE
    :out("True\n"),
    'an unrecognized boundary no-ops inside an alternation before 6.e';

# vim: expandtab shiftwidth=4
