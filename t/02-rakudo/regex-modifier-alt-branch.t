use v6.e.PREVIEW;
use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;
use nqp;

plan 40;

# An internal regex modifier such as `:i` or `:dba` is not an atom. A
# regex, or a branch of an alternation or conjunction, made up of nothing
# but modifiers would match the empty string, so from 6.e on it is refused
# at compile time. Earlier language revisions keep the empty match.

throws-like ｢'BAZ' ~~ / :i | foo | bar /｣, X::Syntax::Regex::SolitaryModifier,
    :modifiers<:i>, :branch({ .so }),
    ':i as the first branch of `|` alternation is refused';
throws-like ｢'BAZ' ~~ / :i | foo | bar /｣, X::Syntax::Regex::NullRegex,
    ':i as the first branch is refused as a null regex';
throws-like ｢'BAZ' ~~ / :i || foo || bar /｣, X::Syntax::Regex::SolitaryModifier,
    :modifiers<:i>, :branch({ .so }),
    ':i as the first branch of `||` alternation is refused';
throws-like ｢'A' ~~ / :i & <[ A..Z ]> & a /｣, X::Syntax::Regex::SolitaryModifier,
    :modifiers<:i>, :branch({ .so }),
    ':i as the first branch of `&` conjunction is refused';
throws-like ｢'A' ~~ / :i && <[ A..Z ]> && a /｣, X::Syntax::Regex::SolitaryModifier,
    :modifiers<:i>, :branch({ .so }),
    ':i as the first branch of `&&` conjunction is refused';
throws-like ｢'ZZZ' ~~ / a | :i | b /｣, X::Syntax::Regex::SolitaryModifier,
    :modifiers<:i>, :branch({ .so }),
    ':i as a middle branch is refused';
throws-like ｢'ZZZ' ~~ / a | b | :i /｣, X::Syntax::Regex::SolitaryModifier,
    :modifiers<:i>, :branch({ .so }),
    ':i as the last branch is refused';
throws-like ｢'ZZZ' ~~ / :i :r | foo | bar /｣, X::Syntax::Regex::SolitaryModifier,
    :modifiers<:i :r>, :branch({ .so }),
    'several modifiers as a branch are refused and all named';
throws-like ｢'foo' ~~ / :!i | foo | bar /｣, X::Syntax::Regex::SolitaryModifier,
    :modifiers<:!i>, :branch({ .so }),
    'a negated modifier as a branch is refused';
throws-like ｢'foo' ~~ / :i(0) | foo | bar /｣, X::Syntax::Regex::SolitaryModifier,
    :modifiers<:i(0)>, :branch({ .so }),
    'a modifier with an argument as a branch is refused and named as written';
throws-like ｢'foo' ~~ / :ignorecase | foo | bar /｣, X::Syntax::Regex::SolitaryModifier,
    :modifiers<:ignorecase>, :branch({ .so }),
    'a long form modifier as a branch is refused';
throws-like ｢'foo' ~~ / :i: | foo | bar /｣, X::Syntax::Regex::SolitaryModifier,
    :modifiers<:i>, :branch({ .so }),
    'a modifier with a backtrack marker as a branch is refused';
todo 'the legacy frontend parses :dba(...) as :dba and a capture group'
    unless nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';
throws-like ｢'ZZZ' ~~ / :dba('branches') | foo | bar /｣, X::Syntax::Regex::SolitaryModifier,
    :modifiers([":dba('branches')"]), :branch({ .so }),
    ':dba as a branch is refused';
throws-like ｢'ZZZ' ~~ / :i | foo & <[ a..z ]>+ | bar /｣, X::Syntax::Regex::SolitaryModifier,
    :modifiers<:i>, :branch({ .so }),
    ':i as a branch of an alternation over conjunctions is refused';
throws-like ｢'ZZZ' ~~ / [ :i | foo | bar ] /｣, X::Syntax::Regex::SolitaryModifier,
    :modifiers<:i>, :branch({ .so }),
    ':i as a branch inside a group is refused';
throws-like ｢grammar G { token t { :i | foo | bar } }｣, X::Syntax::Regex::SolitaryModifier,
    :modifiers<:i>, :branch({ .so }),
    ':i as a branch in a grammar token is refused';
throws-like ｢grammar R { rule r { :i | foo | bar } }｣, X::Syntax::Regex::SolitaryModifier,
    :modifiers<:i>, :branch({ .so }),
    ':i as a branch in a grammar rule is refused';
throws-like ｢'foo' ~~ / :i /｣, X::Syntax::Regex::SolitaryModifier,
    :modifiers<:i>, :branch({ !.so }),
    'a regex holding only :i is refused';
throws-like ｢'foo' ~~ / :i: /｣, X::Syntax::Regex::SolitaryModifier,
    :modifiers<:i>, :branch({ !.so }),
    'a regex holding only a modifier with a backtrack marker is refused';
throws-like ｢'foo' ~~ / :i :s /｣, X::Syntax::Regex::SolitaryModifier,
    :modifiers<:i :s>, :branch({ !.so }),
    'a regex holding only modifiers is refused';
todo 'the legacy frontend parses :dba(...) as :dba and a capture group'
    unless nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';
throws-like ｢'foo' ~~ / :dba('two words') /｣, X::Syntax::Regex::SolitaryModifier,
    :modifiers([":dba('two words')"]), :branch({ !.so }),
    message => { .contains('modifier :dba') },
    'a regex holding only :dba is refused as a single modifier';
throws-like ｢'foo' ~~ / [ :i ] foo /｣, X::Syntax::Regex::SolitaryModifier,
    :modifiers<:i>, :branch({ !.so }),
    'a group holding only :i is refused';
throws-like ｢'foo' ~~ / ( :i ) foo /｣, X::Syntax::Regex::SolitaryModifier,
    :modifiers<:i>, :branch({ !.so }),
    'a capture group holding only :i is refused';
throws-like ｢'foo' ~~ / foo <?before :i> /｣, X::Syntax::Regex::SolitaryModifier,
    :modifiers<:i>, :branch({ !.so }),
    'a lookahead holding only :i is refused';

{
    try EVAL ｢'foo' ~~ / :i | :s /｣;
    isa-ok $!, X::Comp::Group,
        'branches that are all modifiers are each refused';
    like $!.message, / ':i [ ... ]' .* ':s [ ... ]' /,
        'each of the modifier-only branches is named in its own sorry';
}

{
    try EVAL ｢'foo' ~~ / foo | /｣;
    isa-ok $!, X::Syntax::Regex::NullRegex,
        'an empty branch is refused as a null regex';
    unlike $!.message, / 'regex modifier' /,
        'an empty branch is not reported as a modifier';
}

is 'foo' ~~ / :my $x; | foo | bar /, 'foo',
    'a declaration alone as a branch is an atom and compiles';
is 'foo' ~~ / { } | foo | bar /, 'foo',
    'a code block alone as a branch is an atom and compiles';
is 'foo' ~~ / :i { } | foo | bar /, 'foo',
    'a modifier followed by a code block is not modifier only';
is 'FOO' ~~ m:i/ | foo | bar /, 'FOO',
    'an external adverb with a leading | is not a modifier only branch';
is 'FOO' ~~ / :i foo | bar /, 'FOO',
    'a modifier followed by an atom in the first branch compiles';

like X::Syntax::Regex::SolitaryModifier.new(:modifiers[':i'], :branch).message,
    / ':i [ ... ]' /,
    'the branch message shows how to apply the modifier to every branch';
like X::Syntax::Regex::SolitaryModifier.new(:modifiers[':i', ':s'], :branch).message,
    / 'modifiers :i :s do not' .* 'apply' \s+ 'them' /,
    'the branch message is plural for several modifiers';
unlike X::Syntax::Regex::SolitaryModifier.new(:modifiers[':i'], :!branch).message,
    / '[ ... ]' /,
    'the whole regex message does not talk about branches';

is-deeply 'BAZ' ~~ / :i [ | foo | bar ] /, Nil,
    ':i before a grouped alternation does not match the empty string';
is 'BAR' ~~ / :i [ | foo | bar ] /, 'BAR',
    ':i before a grouped alternation applies to every branch';

# A `use v6.d` inside an EVAL does not lower the language revision on
# every frontend, so the 6.d behaviour gets its own process.
is-run q:to/CODE/,
    use v6.d;
    print 'BAZ' ~~ / :i | foo | bar /;
    CODE
    :out(''),
    'under 6.d a modifier-only branch compiles and matches the empty string';
is-run q:to/CODE/,
    use v6.d;
    print 'foo' ~~ / :i /;
    CODE
    :out(''),
    'under 6.d a modifier-only regex compiles and matches the empty string';

# vim: expandtab shiftwidth=4
