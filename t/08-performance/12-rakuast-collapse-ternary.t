use lib <t/packages/Test-Helpers>;
use Test::Helpers;
use Test;
use experimental :rakuast;

plan 7;

# A ternary with a constant condition collapses to the branch the condition
# selects. Each scenario asserts the shape of the tree the program compiles
# to, through Str.AST, which runs the same parse, check, and optimize the
# compiler runs.
my sub tree(Str $source) { $source.AST.DEPARSE }

ok tree(Q[my $x = 1 ?? 10 !! 20]).contains('= 10'),
    'a true constant condition selects the then branch';
ok tree(Q[my $x = 0 ?? 10 !! 20]).contains('= 20'),
    'a false constant condition selects the else branch';
ok tree(Q[my $x = "0" ?? "t" !! "f"]).contains('= "t"'),
    'the condition uses Raku truthiness, where a non-empty string is true';

# The collapse declines when the condition's truth is not a known constant.
ok tree(Q[my $x = Int ?? "t" !! "f"]).contains('??'),
    'a type object condition is left for runtime';
ok tree(Q[my $a = 1; my $x = $a ?? "t" !! "f"]).contains('??'),
    'a runtime condition is left for runtime';
ok tree(Q[my constant C = class :: is Cool { method Bool { die } }.new; my $x = C ?? 1 !! 2]).contains('??'),
    'a condition whose truthiness throws is left for runtime';

# The unoptimized tree still holds the ternary, so the scenarios above are
# known to be testing the optimizer rather than something upstream.
is-run 'use experimental :rakuast; print Q[my $x = 1 ?? 10 !! 20].AST.DEPARSE',
    'without optimization the ternary survives',
    :compiler-args['--optimize=off'], :out(*.contains('??'));

# vim: expandtab shiftwidth=4
