use lib <t/packages/Test-Helpers>;
use Test::Helpers::QAST;
use Test;
use QAST:from<NQP>;
use nqp;
plan 14;

# A conditional or loop whose condition computes a native int branches
# on that int directly: the boolification over an inlined native
# comparison is dropped, and a bare native-int variable is compared
# against zero instead of boxing a Bool every time around.

# The comparison shapes depend on this frontend's compile-time dispatch
# inlining putting the boolified native comparison in condition position.
if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    qast-is 'my int $i; while $i < 5 { $i++ }', :full, -> \v {
        qast-contains-op(v, 'islt_i') and not qast-contains-op(v, 'hllbool')
    }, 'a native comparison while condition branches on islt_i with no boolification';

    qast-is 'my int $i = 3; if $i < 5 { say 1 }', :full, -> \v {
        qast-contains-op(v, 'islt_i') and not qast-contains-op(v, 'hllbool')
    }, 'a native comparison if condition branches on islt_i with no boolification';

    qast-is 'my int $i = 3; unless $i > 5 { say 1 }', :full, -> \v {
        qast-contains-op(v, 'isgt_i') and not qast-contains-op(v, 'hllbool')
    }, 'a native comparison unless condition branches on isgt_i with no boolification';

    qast-is 'my int $i = 3; my int $k = 0; $k++ while $i-- > 0; say $k', :full, -> \v {
        not qast-contains-op(v, 'hllbool')
    }, 'a while statement modifier condition loses its boolification';
}
else {
    skip 'the stripped comparison shapes are specific to the RakuAST frontend', 4;
}

qast-is 'my int $i = 3; while $i { $i-- }', :full, -> \v {
    qast-contains-op(v, 'isne_i')
}, 'a bare native-int while condition compares against zero';

qast-is 'my $x = 3; while $x { $x-- }', :full, -> \v {
    not qast-contains-op(v, 'isne_i')
}, 'a boxed variable condition stays as it was';

# Behavior stays identical.

{
    my int $i = 0;
    my int $n = 0;
    while $i < 5 { $n = $n + $i; $i++ }
    is $n, 10, 'a stripped while comparison loops the right number of times';
}

{
    my int $i = 3;
    while $i { $i-- }
    is $i, 0, 'a bare native-int condition counts down to zero';
}

{
    my int $i = -1;
    my $r = $i ?? 'true' !! 'false';
    while $i { $i++ }
    is $i, 0, 'a negative native int is true and increments up to zero';
}

{
    my int $i = 3;
    is ($i < 5 ?? 'lt' !! 'ge'), 'lt', 'ternaries are untouched and correct';
    my @r;
    if $i < 2 { @r.push('a') } elsif $i < 5 { @r.push('b') } else { @r.push('c') }
    is @r.join, 'b', 'a stripped elsif chain picks the right branch';
}

{
    my int $i = 5;
    my @r;
    @r.push('if') if $i < 9;
    @r.push('unless') unless $i < 2;
    is @r.join(','), 'if,unless', 'stripped condition modifiers still fire correctly';
}

{
    my int $j = 0;
    repeat { $j++ } while $j < 3;
    is $j, 3, 'a stripped repeat-while runs its body first';
}

{
    my $seen;
    with 42 { $seen = $_ }
    is $seen, 42, 'a with part still topicalizes its condition value';
}
