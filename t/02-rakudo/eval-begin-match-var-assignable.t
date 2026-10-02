use Test;

# A BEGIN or CHECK block inside EVAL code gets its own $/, which must be
# a container that the block and anything it calls can assign to.

plan 5;

use MONKEY-SEE-NO-EVAL;

is EVAL(q:to/CODE/), 1,
        BEGIN { $/ = 1; $/ }
        CODE
    'a BEGIN block in EVAL code assigns to $/';

is EVAL(q:to/CODE/), 'b',
        BEGIN { 'ab' ~~ /b/; ~$/ }
        CODE
    'a match in a BEGIN block in EVAL code sets $/';

is EVAL(q:to/CODE/), 'a',
        grammar G { token TOP { 'a' } }
        BEGIN ~G.parse('a')
        CODE
    'a grammar parse in a BEGIN block in EVAL code';

is EVAL(q:to/CODE/), 'b',
        my $r;
        CHECK { 'ab' ~~ /b/; $r = ~$/ }
        $r
        CODE
    'a match in a CHECK block in EVAL code sets $/';

is EVAL(q:to/CODE/), 'xb',
        BEGIN { my $s = 'ab'; $s ~~ s/a/x/; $s }
        CODE
    'a substitution in a BEGIN block in EVAL code';

# vim: expandtab shiftwidth=4
