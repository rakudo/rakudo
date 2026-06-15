use lib <t/packages/Test-Helpers>;
use Test::Helpers;
use Test;
use experimental :rakuast;

plan 13;

# A boolean short-circuit with a constant left operand collapses to the side
# the operator yields, and the dropped side's code is removed. Each scenario
# asserts the shape of the tree the program compiles to, through Str.AST,
# which runs the same parse, check, and optimize the compiler runs.
my sub tree(Str $source) { $source.AST.DEPARSE }

ok tree(Q[my $y = True && 7]).contains('= 7'),
    'a true left operand of && yields the right side';
{
    my $t = tree(Q[sub e() {9}; my $y = False && e()]);
    ok $t.contains('= False'), 'a false left operand of && yields itself';
    nok $t.contains('&&'), 'the dropped side is gone from the tree';
}
{
    my $t = tree(Q[sub e() {9}; my $y = 5 || e()]);
    ok $t.contains('= 5'), 'a true left operand of || yields itself';
    nok $t.contains('||'), 'the dropped side of || is gone from the tree';
}
ok tree(Q[my $y = (True and 7)]).contains('= 7'),
    'the loose form and collapses the same way';
ok tree(Q[my $y = (False or 7)]).contains('= 7'),
    'the loose form or collapses the same way';

# The collapse declines when the left operand's truth is not a known
# constant, when dropping the other side would lose a declaration, or when
# the left operand itself declares one.
ok tree(Q[my $a = 1; my $y = $a && 7]).contains('&&'),
    'a runtime left operand is left for runtime';
ok tree(Q[sub e() {9}; my $y = (my @a := (3, 7)) && e()]).contains('&&'),
    'a left operand that declares a variable is left for runtime';
ok tree(Q[my $y = False && (my $x = 5)]).contains('&&'),
    'a side holding a declaration keeps the branch';
nok tree(Q[my $y = False && do { my $t = 5; $t }]).contains('do'),
    'a side whose declarations are confined to a block of its own is dropped';
ok tree(Q[my $y = False && sub zkeep($a) { $a }]).contains('sub'),
    'a side declaring a named sub is kept';

# The unoptimized tree still holds the operator, so the scenarios above are
# known to be testing the optimizer rather than something upstream.
is-run 'use experimental :rakuast; print Q[my $y = True && 7].AST.DEPARSE',
    'without optimization the short-circuit survives',
    :compiler-args['--optimize=off'], :out(*.contains('&&'));

# vim: expandtab shiftwidth=4
