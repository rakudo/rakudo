use Test;
use nqp;

# A parameter read carries its declared type into the compile time call
# analysis, so a call that can never bind is refused whether the
# argument is a typed variable or a parameter. A definite argument type
# is judged by its base type. Definedness stays a run time check.

plan 19;

sub compile-refuses($code, $expected, $desc) {
    my $error = '';
    {
        EVAL $code;
        CATCH { default { $error = .gist.Str } }
    }
    ok $error.contains($expected), $desc;
}

sub compiles($code, $desc) {
    my $error = '';
    {
        EVAL $code;
        CATCH { default { $error = .gist.Str } }
    }
    is $error, '', $desc;
}

compile-refuses 'sub g(Int $x) { }; sub f(Str $s) { g($s) }',
    'will never work',
    'a call through a read-only parameter that can never bind is refused at compile time';

compile-refuses 'sub g(Int $x) { }; sub f(Str $s is copy) { g($s) }',
    'will never work',
    'a call through a copy parameter that can never bind is refused at compile time';

compile-refuses 'sub g(Int $x) { }; sub f(Str $s is rw) { g($s) }',
    'will never work',
    'a call through an rw parameter that can never bind is refused at compile time';

compile-refuses 'sub g(Int $x) { }; sub f(Str $s is raw) { g($s) }',
    'will never work',
    'a call through a raw parameter that can never bind is refused at compile time';

compiles 'sub g(Int $x) { }; sub f($s) { g($s) }',
    'an untyped parameter argument is not judged at compile time';

compiles 'sub g(Cool $x) { }; sub f(Int $i) { g($i) }',
    'a parameter argument that can bind compiles';

compiles 'sub g($x) { }; sub f() { my Mu:D $i = 5; g($i) }',
    'a definite Mu variable argument is not refused';

compiles 'sub g($x) { }; sub f(Mu:D $v) { g($v) }',
    'a definite Mu parameter argument is not refused';

{
    sub g($x) { $x }
    sub f(Mu:D $v) { g($v) }
    is f(5), 5, 'a definite Mu parameter argument still passes its value';
}

compile-refuses 'my subset S of Str; sub g(Int $x) { }; sub f(S $s) { g($s) }',
    'will never work',
    'a subset parameter argument is judged by its refinee';

compiles 'my subset S of Str; sub g(Str $x) { }; sub f(S $s) { g($s) }',
    'a subset parameter argument binds where its refinee binds';

compile-refuses 'my subset S of Str:D; sub g(Int $x) { }; sub f(S $s) { g($s) }',
    'will never work',
    'a subset of a definite type strips to its nominal foundation';

compile-refuses 'sub g(Int $x) { }; sub f(Str:D $s) { g($s) }',
    'will never work',
    'a definite typed parameter argument is judged by its base type';

compile-refuses 'multi g(Int $x) { }; multi g(Rat $x) { }; sub f(Str $s) { g($s) }',
    'will never work',
    'a parameter argument no multi candidate can bind is refused at compile time';

compile-refuses 'multi g(Int $x) { }; multi g(Rat $x) { }; sub f(Str $s) { g($s) }',
    'any of these multi signatures:',
    'refusing a multi call renders the candidate list';

compile-refuses 'multi g(Int $x) { }; multi g(Rat $x) { }; sub f(Str $s) { g($s) }',
    '(Rat $x)',
    'the candidate list names each signature';

compiles 'sub g(Str $x) { }; sub f(Int() $v) { g($v) }',
    'a coercion typed parameter argument is not judged at compile time';

compiles 'sub g(Cool $x) { }; sub f(Int:D $i) { g($i) }',
    'a definite typed parameter argument binds where its base type binds';

# The legacy frontend does not judge a definite typed variable
# argument, so that refusal is pinned to RakuAST.
if nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast' {
    compile-refuses 'sub g(Str $x) { }; sub f() { my Int:D $i = 5; g($i) }',
        'will never work',
        'a definite typed variable argument is judged by its base type';
}
else {
    skip 'the legacy frontend does not judge definite typed variable arguments', 1;
}

# vim: expandtab shiftwidth=4
