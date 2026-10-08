use Test;
use MONKEY-SEE-NO-EVAL;

plan 9;

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
