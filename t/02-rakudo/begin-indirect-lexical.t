use lib <t/02-rakudo/test-packages>;
use Test;
use nqp;

plan 32;

# An indirect lookup performs its lookup when the expression evaluates. At
# BEGIN time that evaluation happens in a thunk the compiler builds and runs,
# whose outer is the setting, so a pseudo-stash walking that thunk's contexts
# reaches setting symbols but nothing the compiling unit declares. The
# compiler answers for the unit's own declarations, and only for those: a
# unit-local name that came back as a "no such symbol" Failure instead would
# hold compiler frames in its backtrace and break precompilation, and an
# answer handed to foreign code would let a loaded unit read scopes it
# cannot see on either frontend.

my sub script-hex($x) { "script-" ~ $x }
my $script-value = 7;
class ScriptClass { method which() { "script-class" } }

is (BEGIN ::('&script-hex'))("a"), 'script-a',
    'a BEGIN indirect lookup finds a sub declared in the compiling unit';

is (BEGIN ::('&sprintf'))("%d", 3), '3',
    'a BEGIN indirect lookup still finds a setting sub';

is (BEGIN ::('ScriptClass')).which, 'script-class',
    'a BEGIN indirect lookup finds a class declared in the compiling unit';

is (BEGIN ::('$script-value')), 7,
    'a BEGIN indirect lookup of a scalar reaches the container the unit assigns at run time';

my constant $constant-sub = ::('&script-hex');
is $constant-sub("b"), 'script-b',
    'a constant initialized from an indirect lookup holds the unit-local sub';

is ::('&script-hex')("c"), 'script-c',
    'a run time indirect lookup of a unit-local sub keeps working';

# A name built at run time names nothing in the tree, so the lookup has only
# the walk and what the compiler can still answer for.
is (BEGIN ::("&" ~ "script-hex"))("f"), 'script-f',
    'a BEGIN indirect lookup built from a computed string finds a unit-local sub';

# The lookup site sits below the frame the compiler builds for the BEGIN
# code, so the answer must hold at any depth, not only in that frame.
my sub via-helper() { ::('&script-hex')('k') }
is (BEGIN via-helper()), 'script-k',
    'an indirect lookup in a sub the BEGIN calls finds a unit-local sub';

my sub via-lexical-helper() { LEXICAL::<&script-hex>('l') }
is (BEGIN via-lexical-helper()), 'script-l',
    'a LEXICAL:: lookup in a sub the BEGIN calls finds a unit-local sub';

# The lookup must reach the variable's own container, not a copy of it.
my $begin-set;
BEGIN ::('$begin-set') = 42;
is $begin-set, 42,
    'assigning through a BEGIN indirect lookup reaches the unit variable';

# A name nothing declares consults the resolver and still misses. The
# miss is boolified in place: a try here would write the exception into
# a serialized $! container and break the compilation on both frontends.
nok (BEGIN ?::('&no-such-name')),
    'a BEGIN indirect lookup of an undeclared name still comes up empty';

nok (BEGIN LEXICAL::.EXISTS-KEY('&no-such-name')),
    'LEXICAL:: at BEGIN time still reports an undeclared name as absent';

is (BEGIN ::("Script" ~ "Class")).which, 'script-class',
    'a BEGIN indirect lookup built from a computed string finds a unit-local class';

# The same lookups inside a module, which precompiles and so has to serialize
# whatever the constants ended up holding.
use BeginIndirectLexical;

is $BeginIndirectLexical::local-sub("d"), 'local-d',
    'a precompiled module constant holds a sub the module itself declared';

is $BeginIndirectLexical::imported-sub("e"), 'exported-e',
    'a precompiled module constant holds a sub the module imported';

is $BeginIndirectLexical::setting-sub("%d", 4), '4',
    'a precompiled module constant holds a setting sub';

is $BeginIndirectLexical::local-class.which, 'local-class',
    'a precompiled module constant holds a class the module itself declared';

is $BeginIndirectLexical::computed-sub("g"), 'exported-g',
    'a precompiled module constant holds a sub found through a computed string';

is $BeginIndirectLexical::missing-lookup, False,
    'a precompiled module constant holds the handled miss of an undeclared name';

# A loaded unit runs its own code while this one is still at BEGIN time. What
# it resolves has to stay its own business.
my sub consumer-only() { 1 }
use BeginIndirectProbe;

is $BeginIndirectProbe::mainline-saw, 'not found',
    'a loaded unit does not reach the loading unit through an indirect lookup';

# Foreign code a begin-time effect runs is in the same position as a loaded
# unit's mainline: its frames are not the compiling unit's, so its indirect
# lookups must not be answered for either.
my sub loader-secret() { 1 }
use BeginIndirectForeign;

is (BEGIN foreign-probe()), 'not found',
    'an imported sub called at BEGIN time does not reach the calling unit';

sub trait-carrier() is begin-indirect-probing { }
is $BeginIndirectForeign::trait-saw, 'not found',
    'an imported trait handler does not reach the unit applying the trait';

# A closure another unit made at its own BEGIN time runs in frames of that
# compilation, which must not pass for frames of this one. The call happens
# inside a BEGIN, so it is compiled through EVAL where the legacy frontend,
# which cannot invoke a precompiled BEGIN-made closure at all, can skip it.
use BeginIndirectClosure;

if nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast' {
    is EVAL('BEGIN BeginIndirectClosure::begin-closure()'), 'not found',
        'a BEGIN-made closure from a loaded unit does not reach the loading unit';
}
else {
    skip 'the legacy frontend cannot invoke a precompiled BEGIN-made closure';
}

# LEXICAL:: walks the same chain an indirect lookup does, so it gets the
# same answer for the compiling unit's code and the same silence for
# foreign code.
ok (BEGIN LEXICAL::.EXISTS-KEY('&script-hex')),
    'LEXICAL:: at BEGIN time knows a sub declared in the compiling unit';

is (BEGIN LEXICAL::<&script-hex>)("h"), 'script-h',
    'LEXICAL:: at BEGIN time reaches the sub itself';

is (BEGIN foreign-lexical-probe()), 'not found',
    'an imported sub cannot see the calling unit through LEXICAL:: at BEGIN time';

# A name without the * twigil walks the static chain through DYNAMIC:: and
# OUTERS:: as well, so those share the gap and the answer.
is (BEGIN DYNAMIC::<&script-hex>)("i"), 'script-i',
    'DYNAMIC:: at BEGIN time reaches a unit sub through its static-chain path';

is (BEGIN OUTERS::<&script-hex>)("j"), 'script-j',
    'OUTERS:: at BEGIN time reaches a sub declared in the compiling unit';

ok (BEGIN DYNAMIC::.EXISTS-KEY('&script-hex')),
    'DYNAMIC:: at BEGIN time knows a sub declared in the compiling unit';

ok (BEGIN OUTERS::.EXISTS-KEY('&script-hex')),
    'OUTERS:: at BEGIN time knows a sub declared in the compiling unit';

is (BEGIN foreign-dynamic-probe()), 'not found',
    'an imported sub cannot see the calling unit through DYNAMIC:: at BEGIN time';

is (BEGIN foreign-outers-probe()), 'not found',
    'an imported sub cannot see the calling unit through OUTERS:: at BEGIN time';

# vim: expandtab shiftwidth=4
