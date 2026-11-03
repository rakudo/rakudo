use lib <t/packages/Test-Helpers>;
use Test::Helpers;
use Test;
use experimental :rakuast;

plan 8;

# A ternary with a constant condition collapses to the branch the condition
# selects. The helper deparses a source after optimizing it, so each
# scenario asserts the collapsed shape.
my sub optimized-deparse(Str $source) {
    my $cu := $source.AST(:compunit);
    $cu.optimize($cu.resolver);
    $cu.DEPARSE
}

ok optimized-deparse(Q[my $x = 1 ?? 10 !! 20]).contains('= 10'),
    'a true constant condition selects the then branch';
ok optimized-deparse(Q[my $x = 0 ?? 10 !! 20]).contains('= 20'),
    'a false constant condition selects the else branch';
ok optimized-deparse(Q[my $x = "0" ?? "t" !! "f"]).contains('= "t"'),
    'the condition uses Raku truthiness, where a non-empty string is true';

# The collapse declines when the condition's truth is not a known constant.
ok optimized-deparse(Q[my $x = Int ?? "t" !! "f"]).contains('??'),
    'a type object condition is left for runtime';
ok optimized-deparse(Q[my $a = 1; my $x = $a ?? "t" !! "f"]).contains('??'),
    'a runtime condition is left for runtime';
ok optimized-deparse(Q[my constant C = class :: is Cool { method Bool { die } }.new; my $x = C ?? 1 !! 2]).contains('??'),
    'a condition whose truthiness throws is left for runtime';

# The condition is removed by the collapse, so it must be droppable. A
# condition that declares a variable keeps its binding.
ok optimized-deparse(Q[my $x = (my @a := (3, 7)) ?? "t" !! "f"]).contains('??'),
    'a condition that declares a variable is left for runtime';

# Str.AST does not optimize, so the ternary survives. The scenarios above
# therefore test the optimize pass, not something upstream.
ok Q[my $x = 1 ?? 10 !! 20].AST.DEPARSE.contains('??'),
    'the unoptimized tree keeps the ternary';

# vim: expandtab shiftwidth=4
