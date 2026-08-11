use v6.e.PREVIEW;
use lib <t/02-rakudo/test-packages>;
use Test;

plan 6;

# The 6.e setting carries its own PseudoStash, so the begin-time lookups
# t/02-rakudo/begin-indirect-lexical.t pins for 6.c need pinning against
# that implementation as well.

my sub script-hex($x) { "script-" ~ $x }

is (BEGIN ::('&script-hex'))("a"), 'script-a',
    'a 6.e BEGIN indirect lookup finds a sub declared in the compiling unit';

ok (BEGIN LEXICAL::.EXISTS-KEY('&script-hex')),
    '6.e LEXICAL:: at BEGIN time knows a sub declared in the compiling unit';

is (BEGIN LEXICAL::<&script-hex>)("b"), 'script-b',
    '6.e LEXICAL:: at BEGIN time reaches the sub itself';

ok (BEGIN OUTERS::.EXISTS-KEY('&script-hex')),
    '6.e OUTERS:: at BEGIN time knows a sub declared in the compiling unit';

nok (BEGIN LEXICAL::.EXISTS-KEY('&no-such-name')),
    '6.e LEXICAL:: at BEGIN time still reports an undeclared name as absent';

# Foreign code a begin-time effect runs stays isolated under 6.e too.
my sub loader-secret() { 1 }
use BeginIndirectForeign;

is (BEGIN foreign-lexical-probe()), 'not found',
    'an imported sub cannot see the calling 6.e unit through LEXICAL:: at BEGIN time';

# vim: expandtab shiftwidth=4
