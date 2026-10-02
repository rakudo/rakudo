use lib <t/packages/Test-Helpers>;
use Test::Helpers::QAST;
use Test;
use nqp;
plan 40;

# An assignment to a native variable yields the native value it stored,
# so an operator taking it as an operand can compile to a raw op.

if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    qast-is 'my int $pos = 1; my int $end = 10; while ($pos = ($pos * 2) + 1) < $end { }', -> \v {
        qast-contains-op(v, 'islt_i') and not qast-contains-call(v, '&infix:«<»')
    }, 'a native int assignment as the left operand of < compiles to islt_i';
    qast-is 'my int $i = 1; my int $j = 2; my $r = ($i = 7) + $j', -> \v {
        qast-contains-op(v, 'add_i') and not qast-contains-call(v, '&infix:<+>')
    }, 'a native int assignment as the left operand of + compiles to add_i';
    qast-is 'my int $i = 1; my int $j = 2; my $r = $j * ($i = 7)', -> \v {
        qast-contains-op(v, 'mul_i') and not qast-contains-call(v, '&infix:<*>')
    }, 'a native int assignment as the right operand of * compiles to mul_i';
    qast-is 'my int $i = 1; my $r = $i < ($i = 7)', -> \v {
        qast-contains-op(v, 'islt_i') and not qast-contains-call(v, '&infix:«<»')
    }, 'the variable read beside its own assignment still compiles to islt_i';
    qast-is 'my int $i = 1; my $b = 5; my $r = ($i = $b) < 9', -> \v {
        qast-contains-op(v, 'islt_i') and not qast-contains-call(v, '&infix:«<»')
    }, 'an assignment of a boxed value to a native int takes the type of the variable';
    qast-is 'my num $n = 1e0; my $r = ($n = 2.5e0) < 3e0', -> \v {
        qast-contains-op(v, 'islt_n') and not qast-contains-call(v, '&infix:«<»')
    }, 'a native num assignment as an operand compiles to islt_n';
    qast-is 'my str $s = "a"; my $r = ($s = "b") lt "c"', -> \v {
        qast-contains-op(v, 'islt_s') and not qast-contains-call(v, '&infix:<lt>')
    }, 'a native str assignment as an operand compiles to islt_s';
    qast-is 'sub f(int $a is copy) { ($a = 4) * 2 }', :full, -> \v {
        qast-contains-op(v, 'mul_i') and not qast-contains-call(v, '&infix:<*>')
    }, 'an assignment to a native is copy parameter compiles to mul_i';
    qast-is 'my $x = 1; my $r = ($x = 5) < 3', -> \v {
        qast-contains-call(v, '&infix:«<»') and not qast-contains-op(v, 'islt_i')
    }, 'an assignment to a boxed variable keeps the operator call';
    qast-is 'sub f(int $a is rw) { ($a = 4) * 2 }', :full, -> \v {
        qast-contains-call(v, '&infix:<*>') and not qast-contains-op(v, 'mul_i')
    }, 'an assignment to a native is rw parameter keeps the operator call';
    qast-is 'my uint $u = 1; my $r = ($u = 3) < 5', -> \v {
        qast-contains-call(v, '&infix:«<»') and not qast-contains-op(v, 'islt_i')
    }, 'an assignment to a native uint keeps the operator call';
    qast-is 'my int $i = 1; my $r = ($i = 7) < 9e0', -> \v {
        qast-contains-call(v, '&infix:«<»') and not qast-contains-op(v, 'islt_i')
    }, 'a native int assignment beside a num literal keeps the operator call';
    qast-is 'my int $i = 1; my $c = True; my $r = ($i = 7 if $c) < 9', -> \v {
        qast-contains-call(v, '&infix:«<»') and not qast-contains-op(v, 'islt_i')
    }, 'a modified assignment statement in parentheses keeps the operator call';
    qast-is 'my int $i = 1; my $r = 1 < ($i = 3) < 5', -> \v {
        qast-contains-call(v, '&infix:«<»') and not qast-contains-op(v, 'islt_i')
    }, 'an assignment as a chain link operand keeps the chain call';

    # Only the RakuAST frontend reads a native operand as the operator
    # binds it, after an assignment beside it ran.
    {
        my int $i = 3;
        is $i + ($i = 7), 14, 'the variable read before its assignment is deferred behind it';
        $i = 3;
        is-deeply $i < ($i = 7), False, 'a comparison reads the variable after the assignment beside it';
        $i = 3;
        is-deeply ($i) < ($i = 7), False, 'a parenthesized read is deferred behind the assignment beside it';
        $i = 3;
        is ($i) + ($i = 7), 14, 'a parenthesized read sees the value the assignment beside it stored';
        sub f(int $a is copy) { ($a) + ++$a }
        is f(3), 8, 'a parenthesized parameter read is deferred behind an increment beside it';
    }

    # A compile-time dispatch settles the same candidate the runtime
    # dispatch would, and an inlined call evaluates the argument once.
    is EVAL('multi g(int $x is rw) { "rw" }; multi g(Int $x) { "Int" }; my int $i = 1; g($i = 7)'), 'Int',
        'an assignment argument is a value, so a native rw candidate is passed over';
    is EVAL('multi g(int $x) { "int" }; multi g(Str $x) { "str" }; my int $i = 1; g($i = 7) ~ $i'), 'int7',
        'an assignment argument selects the native candidate and stores its value';
    is EVAL('sub k(int $x) { $x + $x }; my int $i = 1; my int $n = 0; k($i = ($n = $n + 1)) ~ $n'), '21',
        'an inlined call reading its parameter twice evaluates the assignment argument once';

    # A sunk raw op keeps the assignment it takes as an operand.
    is EVAL('no worries; my int $i = 1; ($i = 7) < 9; $i'), 7,
        'a sunk comparison still runs the assignment';
}
else {
    skip 'compile-time operator decisions are made by the RakuAST frontend', 23;
}

{
    my int $pos = 1;
    my int $end = 10;
    my @seen;
    while ($pos = ($pos * 2) + 1) < $end { @seen.push($pos) }
    is-deeply @seen, [3, 7], 'the loop sees each assigned value in the condition';
    is $pos, 15, 'the variable holds the value that ended the loop';
}
{
    my int $i = 3;
    is ($i = 7) + $i, 14, 'the variable read after its assignment sees the new value';
    $i = 3;
    is-deeply ($i = 7) < $i, False, 'a comparison reads the variable after the assignment before it';
    $i = 1;
    my $b = 5;
    is-deeply ($i = $b) < 9, True, 'an assignment of a boxed value compares as the stored value';
    is $i, 5, 'the boxed value was stored';
}
{
    my int $i = 1;
    my $x = ($i = 5);
    is $x, 5, 'the assignment still yields its value to a boxed variable';
    is ($i = 6).WHAT, Int, 'the assignment still yields an Int object where one is wanted';
    my int $j = 2;
    is-deeply ($i = 9) > $j, True, 'a comparison of two natives with an assigned left operand';
}
{
    my int $i = 1;
    multi g(int $x) { 'int' }
    multi g(Str $x) { 'str' }
    is g($i = 7), 'int', 'an assignment as a call argument dispatches to the native candidate';
    is $i, 7, 'the call argument assignment stored its value';
}
{
    my int $i = 1;
    is-deeply 1 < ($i = 3) < 5, True, 'a chain with an assigned middle link compares its value';
    is-deeply 4 < ($i = 3) < ($i = 5), False, 'a failed chain link skips the assignment after it';
    is $i, 3, 'the skipped assignment did not run';
}
{
    my num $n = 1e0;
    is-deeply ($n = 2.5e0) < 3e0, True, 'a native num assignment compares as its value';
    my str $s = "a";
    is-deeply ($s = "b") lt "c", True, 'a native str assignment compares as its value';
    my uint $u = 1;
    is-deeply ($u = 3) < 5, True, 'a native uint assignment compares as its value';
}
