use Test;

plan 7;

# A parameterized type used as a type constraint is parameterized at BEGIN
# time, so each argument must have a value by then. A lexical routine does,
# whether passed positionally or as the value of a named argument.

is EVAL(q:to/CODE/), 3, 'a named argument with a routine value in parentheses';
    my role R[$of, :&coerce] { method c { &coerce } }
    sub cd($x) { $x + 1 }
    my R[Int, :coerce(&cd)] $x;
    $x.c.(2)
    CODE

is EVAL(q:to/CODE/), 3, 'a named argument with a routine value as a fat arrow pair';
    my role R[$of, :&coerce] { method c { &coerce } }
    sub cd($x) { $x + 1 }
    my R[Int, coerce => &cd] $x;
    $x.c.(2)
    CODE

is EVAL(q:to/CODE/), 3, 'a named argument with a routine value in nested parentheses';
    my role R[$of, :&coerce] { method c { &coerce } }
    sub cd($x) { $x + 1 }
    my R[Int, :coerce((&cd))] $x;
    $x.c.(2)
    CODE

is EVAL(q:to/CODE/), 3, 'a positional argument with a routine value in parentheses';
    my role R[&coerce] { method c { &coerce } }
    sub cd($x) { $x + 1 }
    my R[(&cd)] $x;
    $x.c.(2)
    CODE

is EVAL(q:to/CODE/), 3, 'an attribute type with a named routine argument';
    my role R[$of, :&coerce] { method c { &coerce } }
    sub cd($x) { $x + 1 }
    my class C { has R[Int, :coerce(&cd)] $.x }
    C.new.x.c.(2)
    CODE

is EVAL(q:to/CODE/), 3, 'a parameter type with a named routine argument';
    my role R[$of, :&coerce] { method c { &coerce } }
    sub cd($x) { $x + 1 }
    sub f(R[Int, :coerce(&cd)] $p?) { $p.c.(2) }
    f()
    CODE

is EVAL(q:to/CODE/), 3, 'a named routine argument given as :&name';
    my role R[$of, :&coerce] { method c { &coerce } }
    sub coerce($x) { $x + 1 }
    my R[Int, :&coerce] $x;
    $x.c.(2)
    CODE

# vim: expandtab shiftwidth=4
