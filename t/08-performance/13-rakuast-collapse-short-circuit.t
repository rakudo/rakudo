use lib <t/packages/Test-Helpers>;
use Test::Helpers;
use Test;
use experimental :rakuast;

plan 13;

# A boolean short-circuit with a constant left operand collapses to the side
# the operator yields, and the dropped side's code is removed. The helper
# deparses a source after optimizing it, so each scenario asserts the collapsed
# shape.
my sub optimized-deparse(Str $source) {
    my $cu := $source.AST(:compunit);
    $cu.optimize($cu.resolver);
    $cu.DEPARSE
}

ok optimized-deparse(Q[my $y = True && 7]).contains('= 7'),
    'a true left operand of && yields the right side';
{
    my $t = optimized-deparse(Q[sub e() {9}; my $y = False && e()]);
    ok $t.contains('= False'), 'a false left operand of && yields itself';
    nok $t.contains('&&'), 'the dropped side is gone from the tree';
}
{
    my $t = optimized-deparse(Q[sub e() {9}; my $y = 5 || e()]);
    ok $t.contains('= 5'), 'a true left operand of || yields itself';
    nok $t.contains('||'), 'the dropped side of || is gone from the tree';
}
ok optimized-deparse(Q[my $y = (True and 7)]).contains('= 7'),
    'the loose form and collapses the same way';
ok optimized-deparse(Q[my $y = (False or 7)]).contains('= 7'),
    'the loose form or collapses the same way';

# The collapse declines when the left operand's truth is not a known
# constant, when dropping the other side would lose a declaration, or when
# the left operand itself declares one.
ok optimized-deparse(Q[my $a = 1; my $y = $a && 7]).contains('&&'),
    'a runtime left operand is left for runtime';
ok optimized-deparse(Q[sub e() {9}; my $y = (my @a := (3, 7)) && e()]).contains('&&'),
    'a left operand that declares a variable is left for runtime';
ok optimized-deparse(Q[my $y = False && (my $x = 5)]).contains('&&'),
    'a side holding a declaration keeps the branch';
nok optimized-deparse(Q[my $y = False && do { my $t = 5; $t }]).contains('do'),
    'a side whose declarations are confined to a block of its own is dropped';
ok optimized-deparse(Q[my $y = False && sub zkeep($a) { $a }]).contains('sub'),
    'a side declaring a named sub is kept';

# Str.AST does not optimize, so the operator survives. The scenarios above
# therefore test the optimize pass, not something upstream.
ok Q[my $y = True && 7].AST.DEPARSE.contains('&&'),
    'the unoptimized tree keeps the short-circuit';

# vim: expandtab shiftwidth=4
