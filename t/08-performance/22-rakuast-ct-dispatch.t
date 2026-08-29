use lib <t/packages/Test-Helpers>;
use Test::Helpers;
use Test::Helpers::QAST;
use Test;
use QAST:from<NQP>;
use nqp;
plan 23;

# A call whose argument types decide the dispatch at compile time is
# replaced by the chosen routine's recorded body, with the argument
# code standing in for the parameters. The recording happens for named
# subs whose parameters are all plain natives and whose body reduces
# to inlinable ops, which covers the native operator candidates the
# setting provides.

qast-is 'my int $i = 3; my int $j = $i + 1; say $j', :full, -> \v {
    qast-contains-op(v, 'add_i') and not qast-contains-call(v, '&infix:<+>')
}, 'native int addition compiles to add_i with no operator call';

qast-is 'my int $i = 3; my $b = $i < 5; say $b', :full, -> \v {
    qast-contains-op(v, 'islt_i') and not qast-contains-call(v, '&infix:«<»')
}, 'a standalone comparison compiles to islt_i with no chain call';

qast-is 'my int $i = 3; my $b = 1 < $i < 5; say $b', :full, -> \v {
    qast-contains-call(v, '&infix:«<»')
}, 'a multi-link chain keeps its chain calls, whose protocol inlining would break';

qast-is 'my num $n = 2e0; my num $m = $n * 3e0; say $m', :full, -> \v {
    qast-contains-op(v, 'mul_n') and not qast-contains-call(v, '&infix:<*>')
}, 'native num multiplication compiles to mul_n with no operator call';

# The recording extends to user subs: a body that itself inlined its
# operator calls reduces to inlinable ops, so its own callers splice it.
# The legacy frontend records inline info before optimizing, so its
# user subs keep the call.
if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    qast-is 'sub twice(int $a) { $a + $a }; my int $i = 2; say twice($i)', :full, -> \v {
        not qast-contains-call(v, '&twice')
    }, 'a call to a native-parameter user sub splices the body in place of the call';
}
else {
    skip 'user-sub inlining through inlined operators is specific to the RakuAST frontend', 1;
}

qast-is 'use soft; sub g(int $a) { $a + 1 }; my int $i = 2; say g($i)', :full, -> \v {
    qast-contains-call(v, '&g')
}, 'the soft pragma keeps the call so the routine stays wrappable';

# A user operator declared after the use shadows the setting candidate
# named by the resolution stored on the node, so the dispatch stays. A
# declaration in an inner block reaches no use outside it. The module
# shows the user's routine running.
qast-is 'my int $i = 3; my int $j = $i + 1; sub infix:<+>(int $a, int $b) { 42 }; say $j', :full, -> \v {
    not qast-contains-op(v, 'add_i')
}, 'a user infix declared after the use keeps native int addition a dispatch';
qast-is 'my int $i = 3; my int $j = $i + 1; multi sub infix:<+>(int $a, int $b) is default { 42 }; say $j', :full, -> \v {
    not qast-contains-op(v, 'add_i')
}, 'a user multi declared after the use keeps native int addition a dispatch';
qast-is 'my int $i = 3; my int $j = $i + 1; my &infix:<+> = sub ($a, $b) { 42 }; say $j', :full, -> \v {
    not qast-contains-op(v, 'add_i')
}, 'an operator bound to a variable after the use keeps native int addition a dispatch';
qast-is 'my int $i = 3; my $b = $i < 5; sub infix:«<»($a, $b) { "user" }; say $b', :full, -> \v {
    not qast-contains-op(v, 'islt_i')
}, 'a user infix declared after the use keeps a native comparison a dispatch';
qast-is 'my int $i = 3; my int $j = $i + 1; { sub infix:<+>(int $a, int $b) { 42 } }; say $j', :full, -> \v {
    qast-contains-op(v, 'add_i')
}, 'a user infix in an inner block leaves a use outside it lowered';
{
    my $dir = make-temp-dir;
    $dir.add('DispatchLateOp.rakumod').spurt: q:to/END/;
        unit module DispatchLateOp;
        our sub add(int $a, int $b) { $a + $b }
        sub infix:<+>(int $a, int $b) { 42 }
        END
    is EVAL(q[use lib $dir; use DispatchLateOp; &DispatchLateOp::add(1, 2)]), 42,
        'a user infix declared after a use in a sub is what the sub dispatches to';
}

# Behavior stays identical where the splice applies.

{
    my int $i = 3;
    is $i + 1, 4, 'inlined native addition computes the right value';
}

{
    my int $i = 3;
    is $i + 170141183460469231731687303715884105727,
        170141183460469231731687303715884105730,
        'an integer literal too wide for a native stays a boxed dispatch';
}

{
    my int $i = 3;
    is-deeply (so any(1, 5) < $i, so all(1, 5) < $i), (True, False),
        'junction arguments still autothread rather than taking a decided dispatch';
}

{
    my int $i = 3;
    is (1 < $i < 5), True, 'a true multi-link chain still short-circuits correctly';
    is (5 < $i < 9), False, 'a false first link stops a multi-link chain';
}

{
    sub bump(int $a is rw) { $a = $a + 1 }
    my int $i = 5;
    bump($i);
    is $i, 6, 'an rw native parameter keeps the call and writes through';
}

{
    use soft;
    sub g(int $a) { $a + 1 }
    g(1);
    &g.wrap(-> |c { 'wrapped' });
    is g(5), 'wrapped', 'wrapping a routine compiled under soft still intercepts calls';
}

{
    multi sub m(int $a) { 'int' }
    multi sub m(str $a) { 'str' }
    my int $i = 1;
    my str $s = 'x';
    is m($i), 'int', 'a decided multi call still lands on the int candidate';
    is m($s), 'str', 'a decided multi call still lands on the str candidate';
}

{
    my int $i = 4;
    is &infix:<+>($i, 2), 6, 'calling the operator through its code object still works';
}

{
    is ([+] 1..5), 15, 'the reduce metaop over an inlinable operator still works';
}
