use Test;
use MONKEY-SEE-NO-EVAL;
use nqp;

plan 28;

is-deeply EVAL(q[constant C = 5.VAR; C]), 5,
    '.VAR on a value in a constant is the value';
is-deeply EVAL(q[constant C = 0 || 5.VAR; C]), 5,
    '.VAR on a value right of || in a constant is the value';
is-deeply EVAL(q[my class A { has $.x is default(True ?? 5.VAR !! 0) }; A.new.x]), 5,
    '.VAR on a value in a ternary trait argument is the value';
is-deeply EVAL(q[constant C = 5.VAR.WHAT; C]), Int,
    '.WHAT of .VAR on a value in a constant is the type of the value';
is EVAL(q[constant C = 5.VAR; C.VAR.^name]), 'Int',
    '.VAR on a value in a constant does not wrap the value in a container';
is EVAL(q[constant C = [1, 2].AT-POS(0).VAR; C.^name]), 'Scalar',
    '.VAR on an array element in a constant is a Scalar';
ok EVAL(q[
        use nqp;
        constant @a = [1, 2];
        constant C = @a.AT-POS(0).VAR;
        nqp::eqaddr(nqp::decont(C), @a.AT-POS(0))
    ]),
    '.VAR on an array element in a constant wraps the container of the element';
ok EVAL(q[use nqp; constant C = Scalar.VAR; nqp::eqaddr(C, Scalar)]),
    '.VAR on a container type object in a constant is the type object';
throws-like q[constant C = 5.?VAR], Exception,
    message => /'Cannot use .? on a non-identifier method call'/,
    '.? with .VAR in a constant reports a non-identifier method call';

is EVAL(q[constant C = 5.?nope; C.raku]), 'Nil',
    '.? in a constant gives Nil for a method the invocant lacks';
is-deeply EVAL(q[my class A { method m($x, :$y) { "$x$y" } }; constant C = A.?m(1, :y(2)); C]), '12',
    '.? in a constant passes the arguments to a method the invocant has';
is-deeply EVAL(q[constant C = [7].AT-POS(0).?is-prime; C]), True,
    '.? in a constant calls a method of the value in a container';
is EVAL(q[my class A { has $.x is default(5.?nope) }; A.new.x.raku]), 'Nil',
    '.? in a trait argument gives Nil for a method the invocant lacks';
is-deeply EVAL(q[
        my class A { method m { "A" } }
        my class B is A { method m { "B" } }
        constant C = B.+m;
        C
    ]), ("B", "A"),
    '.+ in a constant calls each method of the name';
is-deeply EVAL(q[
        my class A { method m { "A" } }
        my class B is A { method m { "B" } }
        my class C { has $.x is default(0 || B.+m) }
        C.new.x
    ]), ("B", "A"),
    '.+ right of || in a trait argument calls each method of the name';
is-deeply EVAL(q[
        my class A { method m($x) { "A$x" } }
        my class B is A { method m($x) { "B$x" } }
        constant C = B.+m(1);
        C
    ]), ("B1", "A1"),
    '.+ in a constant passes the arguments to each method of the name';
is-deeply EVAL(q[
        my class A { method m { "A" } }
        my class B is A { method m { "B" } }
        constant C = B.*m;
        C
    ]), ("B", "A"),
    '.* in a constant calls each method of the name';
is-deeply EVAL(q[
        my class A { method m(:$y) { "A$y" } }
        my class B is A { method m(:$y) { "B$y" } }
        constant C = B.*m(:y(3));
        C
    ]), ("B3", "A3"),
    '.* in a constant passes the named arguments to each method of the name';
is-deeply EVAL(q[constant C = 5.*nope; C]), (),
    '.* in a constant gives an empty list for a method the invocant lacks';
if nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast' {
    is EVAL(q[use nqp; constant L = nqp::list(1, 2); constant C = L.?nope; C.raku]), 'Nil',
        '.? in a constant gives Nil for a method a VM array lacks';
}
else {
    skip 'legacy calls dispatch:<.?>, which a VM array lacks';
}

is EVAL(q[constant C = Int.HOW.mro(Int); C.^name]), 'List',
    'a method call in a constant maps an NQP array it returns to a List';
is EVAL(q[constant C = 0 || Int.HOW.mro(Int); C.^name]), 'List',
    'a method call right of || in a constant maps an NQP array it returns to a List';
is-deeply EVAL(q[constant C = Int.HOW.mro(Int).head; C]), Int,
    'a method call in a constant calls a List method on an NQP array another method call returns';
is EVAL(q[constant C = Int.HOW.?mro(Int); C.^name]), 'List',
    '.? in a constant maps an NQP array it returns to a List';
is EVAL(q[use nqp; constant L = nqp::list(1, 2); constant C = L.WHAT; C.^name]), 'BOOTArray',
    '.WHAT of a VM array in a constant is the VM array type';

ok EVAL(q[use nqp; constant L = nqp::list(1); constant C = L.WHERE; C > 0]),
    '.WHERE of a VM array in a constant is a positive number';
isnt EVAL(q[my class A { method WHERE { 42 } }; constant C = A.WHERE; C]), 42,
    '.WHERE in a constant does not call a WHERE method of the class';
if nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast' {
    is EVAL(q[use nqp; constant L = nqp::list(1); constant C = L.WHERE; C.^name]), 'Int',
        '.WHERE of a VM array in a constant is an Int';
}
else {
    skip 'legacy gives a BOOTInt for .WHERE in a constant';
}
