use Test;
use nqp;

plan 23;

my $rakuast := nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';

is-deeply EVAL(q[comb sum 1000, 2000: 2]), ("30", "00"),
    'colon after a nested listop with comma separated args is the outer listop invocant colon';

is-deeply EVAL(q[join "-", comb sum 1000, 2000: 2]), "30-00",
    'colon goes to the nearest enclosing listop still in its first argument';

is-deeply EVAL(q[comb sum 1000 R, 2000: 2]), ("30", "00"),
    'a reversed comma ends the first argument like a comma';

is-deeply EVAL(q[comb sum 1000 «,» 2000: 2]), ("30", "00"),
    'a hyper comma ends the first argument like a comma';

is-deeply EVAL(q[comb sum 1000 [R,] 2000: 2]), ("30", "00"),
    'a bracketed reversed comma ends the first argument like a comma';

if $rakuast {
    is-deeply EVAL(q[comb ("ab", "cd"): 1]), ("a", "b", " ", "c", "d"),
        'comma inside parentheses does not stop the enclosing listop from taking an invocant colon';

    is-deeply EVAL(q[comb ["ab", "cd"]: 1]), ("a", "b", " ", "c", "d"),
        'comma inside brackets does not stop the enclosing listop from taking an invocant colon';

    is-deeply EVAL(q[comb { "ab", "cd" }(): 1]), ("a", "b", " ", "c", "d"),
        'comma inside a block does not stop the enclosing listop from taking an invocant colon';

    is-deeply EVAL(q[sub f(*@a) { @a.join("|") }; comb &f("ab", "cd"): 1]),
        ("a", "b", "|", "c", "d"),
        'comma inside call arguments does not stop the enclosing listop from taking an invocant colon';

    is-deeply EVAL(q[my class C { method m(*@a) { @a.join("|") }; method t { comb $.m("ab", "cd"): 1 } }; C.t]),
        ("a", "b", "|", "c", "d"),
        'comma inside $.m(...) arguments does not stop the enclosing listop from taking an invocant colon';

    is-deeply EVAL(q[comb "abcd".substr: 1, 2: 1]), ("b", "c"),
        'comma inside method colon arguments does not stop the enclosing listop from taking an invocant colon';

    is-deeply EVAL(q[comb [,]("ab", "cd"): 1]), ("a", "b", " ", "c", "d"),
        'a reduce over comma does not stop the enclosing listop from taking an invocant colon';

    is-deeply EVAL(q[comb &[,]("ab", "cd"): 1]), ("a", "b", " ", "c", "d"),
        'a comma operator reference does not stop the enclosing listop from taking an invocant colon';

    is-deeply EVAL(q[comb -> $a = "ab|cd", $b? { $a }(): 1]), ("a", "b", "|", "c", "d"),
        'comma after a parameter default does not stop the enclosing listop from taking an invocant colon';

    is-deeply EVAL(q[comb -> $a where *.chars, $b? { $a }("ab|cd"): 1]), ("a", "b", "|", "c", "d"),
        'comma after a where clause does not stop the enclosing listop from taking an invocant colon';

    is-deeply EVAL(q[comb try "abc": 1]), ("a", "b", "c"),
        'colon after a try statement prefix in the first argument is the enclosing listop invocant colon';

    is-deeply EVAL(q[comb do "abc": 1]), ("a", "b", "c"),
        'colon after a do statement prefix in the first argument is the enclosing listop invocant colon';
}
else {
    skip 'the legacy frontend lets code nested in the first argument take the invocant colon away from the enclosing listop', 12;
}

todo 'the legacy frontend gives the colon to the method colon arguments' unless $rakuast;
is-deeply EVAL(q[comb "abcd".substr: 1: 2]), ("bc", "d"),
    'colon after method colon arguments is the enclosing listop invocant colon';

throws-like q[comb ("abc": 1)], X::Comp,
    'colon inside parentheses is not the enclosing listop invocant colon';

todo 'the legacy frontend takes the enclosing listop invocant colon inside call arguments'
  unless $rakuast;
throws-like q[sub f(*@a) { @a }; say &f("abc": 1)], X::Comp,
    'colon inside call arguments is not the enclosing listop invocant colon';

is-deeply EVAL(q[my class A { method f(|c) { c } }; sub f(|) { }; f(A: 1; 2)]), \((1,), (2,)),
    'invocant colon in the first of several semicolon separated argument lists makes a method call';

is-deeply EVAL(q[my class A { method f(|c) { c } }; sub f(|) { }; f(A: 1, 2; 3)]), \((1, 2), (3,)),
    'comma separated arguments after an invocant colon are not nested in an extra list';

todo 'the legacy frontend allows an invocant colon after a semicolon' unless $rakuast;
throws-like q[comb("x"; "abc": 2)], X::Comp,
    'invocant colon is not allowed after the first semicolon separated argument list';

# vim: expandtab shiftwidth=4
