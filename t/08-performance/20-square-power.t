use lib <t/packages/Test-Helpers>;
use Test::Helpers::QAST;
use Test;
use nqp;
plan 14;

my $rakuast := nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast';

todo 'the legacy optimizer rewrites a variable square to a multiply' unless $rakuast;
qast-is 'my Int $x = 7; my $y = $x ** 2', -> \v {
        qast-contains-call(v, '&infix:<**>')
    and not qast-contains-call(v, '&infix:<*>')
}, 'a square of a typed variable calls the power routine';
qast-is 'my int $x = 7; my $y = $x ** 2', -> \v {
        qast-contains-op(v, 'pow_i')
    and not qast-contains-call(v, '&infix:<*>')
}, 'a square of a native int variable lowers to the native power op';

{
    my Num $x = 1e-200;
    my num $n = 1e-200;
    todo 'the legacy optimizer squares a variable by a multiply', 2 unless $rakuast;
    fails-like { $x ** 2 }, X::Numeric::Underflow, 'a typed Num square that underflows fails';
    fails-like { $n ** 2 }, X::Numeric::Underflow,
        'a native num square by an Int exponent that underflows fails';
}
{
    my int $x = 7;
    is $x ** 2, 49, 'a native int square computes the power';
}
{
    my int $x = 2 ** 40;
    is $x², 2 ** 80, 'a superscript square of a native int computes the boxed power';
}
{
    my uint $x = 2 ** 40;
    todo 'the legacy optimizer squares a native uint by a wrapping multiply' unless $rakuast;
    is $x ** 2, 2 ** 80, 'a native uint square computes the boxed power';
}

# A junction squares each eigenstate, where a multiply would autothread
# over both operands.
{
    my $untyped = any(1, 2);
    my Junction $typed = any(1, 2);
    my Mu:D $definite = any(1, 2);
    todo 'the legacy optimizer squares a junction by a multiply', 5 unless $rakuast;
    nok ($untyped ** 2) == 2, 'an untyped junction square keeps eigenstate semantics';
    nok ($typed ** 2) == 2, 'a typed junction square keeps eigenstate semantics';
    nok ($definite ** 2) == 2, 'a definite Mu junction square keeps eigenstate semantics';
    nok ($untyped²) == 2, 'a superscript junction square keeps eigenstate semantics';
    is ($typed ** 2).raku, 'any(1, 4)', 'a junction square squares each eigenstate';
}

{
    my $fetches = 0;
    my Int $x := Proxy.new(FETCH => -> $ { ++$fetches }, STORE => -> $, $ { });
    todo 'the legacy optimizer reads the variable once per multiply operand' unless $rakuast;
    ok ($x ** 2).sqrt.narrow ~~ Int, 'a square of a Proxy squares a single fetched value';
}
{
    todo 'the legacy optimizer squares by a user multiply in scope' unless $rakuast;
    is EVAL(q[sub infix:<*>($a, $b) { 42 }; my Int $x = 7; $x ** 2]), 49,
        'a square with a user multiply in scope calls the core power routine';
}

# vim: expandtab shiftwidth=4
