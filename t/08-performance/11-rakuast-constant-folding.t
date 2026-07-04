use lib <t/packages/Test-Helpers>;
use Test::Helpers;
use Test;
use experimental :rakuast;

plan 39;

# Constant folding rewrites a pure operator on constant operands into the
# literal result. The helper deparses a source after optimizing it, so
# each scenario asserts the optimized shape. The decline scenarios assert the
# operator survives where folding must not happen.
my sub optimized-deparse(Str $source) {
    my $cu := $source.AST(:compunit);
    $cu.optimize($cu.resolver);
    $cu.DEPARSE
}

# Operators fold across types and precedence.
ok optimized-deparse(Q[my $y = 2 + 3]).contains('= 5'),          'integer arithmetic folds';
ok optimized-deparse(Q[my $y = 2 * 3 + 4]).contains('= 10'),     'nested operators fold up the chain';
ok optimized-deparse(Q[my $y = 10 - 2 ** 3]).contains('= 2'),    'mixed precedence folds';
ok optimized-deparse(Q[my $y = -2 * 5]).contains('= -10'),       'a negated literal folds';
ok optimized-deparse(Q[my $y = 6 +& 3]).contains('= 2'),         'a bitwise operator folds';
ok optimized-deparse(Q[my $y = "ab" ~ "cd"]).contains('"abcd"'), 'string concatenation folds';
{
    my $t = optimized-deparse(Q[my $y = 2 min 3]);
    ok $t.contains('= 2'), 'a list-associative operator folds';
    nok $t.contains('min'), 'the folded list-associative operator is gone from the tree';
}
ok optimized-deparse(Q[my $j = 1 | 2]).contains('any(1, 2)'),    'a junction constructor folds to a junction';
ok optimized-deparse(Q[my $y = !True]).contains('False'),        'a prefix on an enumeration value folds';
ok optimized-deparse(Q[my $y = 7 ** -1]) ~~ / '¹/₇' | '<1/7>' /, 'a negative power folds to a Rat';
ok optimized-deparse(Q[my $y = 3 / 4]).contains('= 0.75'),       'integer division folds to a Rat';
{ my $y = 3 / 4; isa-ok $y, Rat, 'a folded division still produces a Rat at runtime'; }

# Folding reaches every expression position.
ok optimized-deparse(Q[my $s = (2 ** 10).Str]).contains('1024.Str'),         'a method call invocant folds';
ok optimized-deparse(Q[my $p = (status => 6 * 7)]).contains('status => 42'), 'a pair value folds';
ok optimized-deparse(Q[my $x = do { 100 - 1 }]).contains('99'),              'a block final value folds';
ok optimized-deparse(Q[say "x" if 2 ** 3]).contains('if 8'),                 'a statement modifier condition folds';
ok optimized-deparse(Q[my @a = [2 + 3, 4 * 5]]).contains('[5, 20]'),         'array composer elements fold';
ok optimized-deparse(Q[my @a; @a[2 + 3]]).contains('[5]'),                   'an index expression folds';
ok optimized-deparse(Q[sub f($x = 2 + 3) { }]).contains('= 5'),              'a parameter default folds';
ok optimized-deparse(Q[my $x = 5 * 3 + (2 ** 2) - 10 + 2 * 5]).contains('= 19'),
    'parenthesized constants fold into the enclosing expression';
ok optimized-deparse(Q[my $x = ((2 ** 3))]).contains('= 8'),                 'nested grouping parentheses fold';
ok optimized-deparse(Q[my int $i = 3 * 4]).contains('= 12'),                 'a native int initializer folds';
{ my int $i = 3 * 4; is $i, 12, 'a folded native int initializer widens correctly'; }

# Folding declines where the rewrite would not preserve the program.
ok optimized-deparse(Q[my $a = 5; my $y = $a + 3]).contains('$a + 3'),
    'a variable operand keeps the runtime computation';
ok optimized-deparse(Q[my $c = 1 < 2 < 2]).contains('1 < 2 < 2'),
    'a chained comparison is not mis-folded as binary operations';
ok optimized-deparse(Q[my @a = (1, 2, 3)]).contains('(1, 2, 3)'),
    'list parentheses are not mistaken for a foldable value';

# A constant string repetition folds only when the result is small enough to
# embed in the compilation unit.
ok optimized-deparse(Q[my $y = "ab" x 3]).contains('"ababab"'), 'a small string repetition folds';
ok optimized-deparse(Q[my str $a = "a" x 2**32-1]).contains(' x '),
    'an enormous string repetition is left for runtime';

# Division by zero folds to the same zero-denominator Rat the runtime
# produces, whose use throws just as the unfolded expression would.
ok optimized-deparse(Q[my $y = 1 / 0]) ~~ / '¹/₀' | '<1/0>' /, 'division by zero folds to a zero-denominator Rat';
{ my $y = 1 / 0; dies-ok { $y.Str }, 'using a folded zero-denominator Rat throws' }

# Declining a Failure result, the way 1 div 0 produces one, marks it handled
# so compiling the program stays warning free. The runtime Failure is marked
# handled explicitly: a garbage collection may otherwise legitimately collect
# the abandoned Failure mid-run and warn, which is not the compile-time
# behavior this asserts.
is-run 'my $y = 1 div 0; $y.handled = True; print "compiled"',
    'a declined Failure does not warn at compile time',
    :out<compiled>, :err('');

# A throw during compile-time evaluation declines the fold, keeping the
# throw at runtime where the program put it.
{
    sub infix:<zdie>($a, $b) is pure { die "zdie reached runtime" }
    my $threw = False;
    my $y = do { 1 zdie 2; CATCH { default { $threw = True } } };
    ok $threw, 'a throwing pure operator is left to throw at runtime';
}

# An operator bound to a lexical variable resolves to a declaration with no
# compile-time value, so folding declines and the runtime binding is used.
ok optimized-deparse(Q[my &infix:<zlex> = sub ($a, $b) { $a + $b }; my $y = 1 zlex 2]).contains('1 zlex 2'),
    'an operator bound to a lexical variable is left for runtime';

# Only a routine marked is pure may run at compile time. An unmarked
# user-defined operator runs at runtime: if it were folded, the call would
# run during compilation against the not-yet-initialized counter and the
# runtime count would be zero.
{
    my $count = 0;
    sub infix:<zimpure>($a, $b) { $count++; $a + $b }
    is (4 zimpure 5), 9, 'an unmarked user-defined operator gives the right value';
    is $count, 1, 'an unmarked user-defined operator runs at runtime';
}

# A wrapped pure operator still gives the original result for literal
# operands, because the call was folded away before the wrap took effect at
# runtime.
{
    sub infix:<zshape>($a, $b) is pure { $a + $b }
    &infix:<zshape>.wrap(-> $a, $b { 999 });
    is (2 zshape 3), 5, 'an is pure operator on literals is evaluated at compile time';
}

# An adverb on the operator is a named argument folding would drop, so even
# a pure operator carrying one is left for runtime.
{
    sub prefix:<zfold>($a, :$x) is pure { join(",", $a, $x // "U") }
    is (zfold 4 :x(5)), '4,5', 'a pure prefix operator with an adverb keeps its adverb';
}

# Str.AST does not optimize, so the operator survives. The scenarios above
# therefore test the optimize pass, not something upstream.
ok Q[my $x = 2 + 3].AST.DEPARSE.contains('2 + 3'),
    'the unoptimized tree keeps the operator';

# vim: expandtab shiftwidth=4
