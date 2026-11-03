use lib <t/packages/Test-Helpers>;
use Test::Helpers::QAST;
use Test;
use QAST:from<NQP>;
use nqp;
plan 26;

# A native value parameter binds its register straight through: no
# box, no hllize dispatch, and no unbox on the way to the target.
# Each shape asserts an op from the method body, so a tree that lost
# the body cannot pass. The shapes are this frontend's.

if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    qast-is 'my class T { method m(int $i) { $i + 1 } }; say T.new.m(2)', :full, -> \v {
        qast-contains-op(v, 'add_i')
        and not qast-contains-op(v, 'box_i')
    }, 'a native int parameter binds without boxing';

    qast-is 'my class T { method m(num $n) { $n + 1e0 } }; say T.new.m(1e0)', :full, -> \v {
        qast-contains-op(v, 'add_n')
        and not qast-contains-op(v, 'box_n')
    }, 'a native num parameter binds without boxing';

    qast-is 'use nqp; my class T { method m(str $s) { nqp::concat($s, "!") } }; say T.new.m("x")', :full, -> \v {
        qast-contains-op(v, 'concat')
        and not qast-contains-op(v, 'box_s')
    }, 'a native str parameter binds without boxing';

    qast-is 'use nqp; my class T { method m(int $i is rw) { $i = nqp::add_i(4,1) } }; my int $x; T.new.m($x); say $x', :full, -> \v {
        qast-contains-op(v, 'add_i')
        and not qast-contains-op(v, 'box_i')
    }, 'an rw native parameter binds its reference without boxing';

    qast-is 'my class T { method m(int $i = 5) { $i + 1 } }; say T.new.m', :full, -> \v {
        qast-contains-op(v, 'add_i') and not qast-contains-op(v, 'box_i')
    }, 'an optional native parameter with a default binds without boxing';

    qast-is 'my class T { method m(int :$i = 3) { $i + 1 } }; say T.new.m(:i(2))', :full, -> \v {
        qast-contains-op(v, 'add_i') and not qast-contains-op(v, 'box_i')
    }, 'a named native parameter binds without boxing';

    qast-is 'use nqp; my class T { method m(int $i is copy) { nqp::add_i($i, 1) } }; say T.new.m(2)', :full, -> \v {
        qast-contains-op(v, 'add_i') and not qast-contains-op(v, 'box_i')
    }, 'a copy native parameter takes its value without boxing';

    qast-is 'my class T { method m(int $i, *@r) { $i + 1 } }; say T.new.m(1, 2, 3)', :full, -> \v {
        qast-contains-op(v, 'add_i') and not qast-contains-op(v, 'box_i')
    }, 'a native parameter beside a slurpy binds without boxing';

    qast-is 'my class T { method m(int8 $i) { $i + 1 } }; say T.new.m(2)', :full, -> \v {
        qast-contains-op(v, 'add_i')
        and not qast-contains-op(v, 'box_i')
    }, 'a sized int parameter binds without boxing';
}
else {
    skip 'the binding shapes are specific to the RakuAST frontend', 9;
}

# Behavior stays identical.

{
    my sub f(int $i) { $i * 2 }
    is f(21), 42, 'a native int parameter passes its value';
    my sub g(num $n) { $n + 0.5e0 }
    is g(1e0), 1.5e0, 'a native num parameter passes its value';
    my sub h(str $s) { $s ~ "!" }
    is h("hi"), 'hi!', 'a native str parameter passes its value';
    my sub r(int $i is rw) { $i = 9 }
    my int $x = 1;
    r($x);
    is $x, 9, 'an rw native parameter writes back to the caller';
    my sub c(int $i is copy) { $i++; $i }
    my int $y = 5;
    is c($y), 6, 'a copy native parameter steps its own copy';
    is $y, 5, 'the copy leaves the caller alone';
    my sub t(int8 $i) { $i + 1 }
    is t(127), 128, 'a sized int parameter computes at full width';
}

{
    my class T { method m(int $i = 5) { $i + 1 } }
    is T.new.m, 6, 'an omitted optional native takes its default';
    is T.new.m(1), 2, 'a passed optional native takes the argument';
}

{
    my class T { method m(int :$i = 3) { $i * 2 } }
    is T.new.m(:i(4)), 8, 'a named native binds a passed named';
    is T.new.m, 6, 'an omitted named native takes its default';
}

{
    my class T {
        multi method m(int $i) { "int:$i" }
        multi method m(str $s) { "str:$s" }
    }
    my int $x = 2;
    my str $s = "x";
    is T.new.m($x), 'int:2', 'a multi dispatches a native int argument';
    is T.new.m($s), 'str:x', 'a multi dispatches a native str argument';
}

{
    my class T { method m(int $i, *@r) { $i + @r.elems } }
    is T.new.m(1, 9, 9), 3, 'a native binds beside a slurpy';
}

{
    my class T { method m(int $i where * > 0) { $i + 1 } }
    is T.new.m(2), 3, 'a where passing native argument binds';
    throws-like { T.new.m(-1) }, Exception, 'a where failing native argument rejects';
}

{
    my class T { method m(int8 $i) { $i + 0 } }
    is T.new.m(300), 44, 'a sized int truncates at bind';
}

# vim: expandtab shiftwidth=4
