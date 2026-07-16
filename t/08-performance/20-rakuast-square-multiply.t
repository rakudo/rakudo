use lib <t/packages/Test-Helpers>;
use Test::Helpers::QAST;
use Test;
use QAST:from<NQP>;
use nqp;
plan 13;

# Squaring by the core power operator becomes a multiply of the operand with
# itself when the operand is a plain variable whose type rules out a
# Junction. Anything else keeps the power routine.

sub qast-op-named (Mu $qast, Str:D $op, Str:D $name --> Bool:D) {
    if nqp::istype($qast, QAST::Op) && $qast.op eq $op && $qast.name eq $name {
        return True;
    }
    elsif qast-descendable $qast {
        for $qast.list {
            qast-op-named $_, $op, $name and return True;
        }
    }
    False
}

# These observe the emitted QAST.
qast-is 'my Int $x = 7; my $y = $x ** 2', -> \v {
        qast-contains-call(v, '&infix:<*>')
    and not qast-contains-call(v, '&infix:<**>')
}, 'squaring a typed variable compiles to a multiply';

qast-is 'my num $x = 2e0; my $y = $x ** 2', -> \v {
        qast-contains-call(v, '&infix:<*>')
    and not qast-contains-call(v, '&infix:<**>')
}, 'squaring a native variable compiles to a multiply';

qast-is 'my Int $x = 7; my $y = $x ** 3', -> \v {
    qast-contains-call(v, '&infix:<**>')
}, 'a different exponent keeps the power routine';

qast-is 'sub f() { 5 }; my $y = f() ** 2', -> \v {
    qast-contains-call(v, '&infix:<**>')
}, 'a call operand keeps the power routine';

qast-is 'sub infix:<**>($a, $b) { 42 }; my Int $x = 7; my $y = $x ** 2', -> \v {
    qast-contains-call(v, '&infix:<**>')
}, 'a user power operator keeps its call';

# These observe that the rewritten square still answers like the power.
{
    my Int $x = 7;
    is $x ** 2, 49, 'a typed Int square computes the power';
}
{
    my num $x = 1.5e0;
    is $x ** 2, 2.25e0, 'a native num square computes the power';
}
{
    my Rat $r = 3/2;
    is $r ** 2, 2.25, 'a typed Rat square computes the power';
}
{
    my Int $x = -3;
    is $x ** 2, 9, 'a negative base squares to a positive';
}
{
    my $x = 7;
    is $x ** 2, 49, 'an untyped variable square computes the power';
}
{
    my constant $K = 6;
    is $K ** 2, 36, 'a sigiled constant square computes the power';
}
{
    my Junction $j = any(1, 2);
    todo 'the legacy optimizer autothreads a junction square wrongly'
        unless nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast';
    nok ($j ** 2) == 2, 'a junction square keeps eigenstate semantics';
}
{
    my Junction $j = any(1, 2);
    ok ($j ** 2) == 4, 'a junction square squares each eigenstate';
}

# vim: expandtab shiftwidth=4
