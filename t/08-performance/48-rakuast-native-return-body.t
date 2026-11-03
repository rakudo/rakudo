use lib <t/packages/Test-Helpers>;
use Test::Helpers;
use Test::Helpers::QAST;
use Test;
use nqp;

plan :skip-all('the native return coercion is elided by the optimize phase') unless optimizer-enabled;
plan :skip-all('these tests observe RakuAST code generation')
    unless nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast';
plan 6;

# A routine with a native return type coerces the value it ends with, but
# a body already in that native form gets no coercion, so it stays small
# enough to record inline info and be spliced into its callers.

qast-is 'sub f(int $a, int $b --> int) { $a + $b }', -> \v {
    !qast-contains-op(v, 'p6decontrv')
}, 'a native int body adds no coercion to a native int return', :full;

qast-is 'sub f(num $a --> num) { $a * 2e0 }', -> \v {
    !qast-contains-op(v, 'p6decontrv')
}, 'a native num body adds no coercion to a native num return', :full;

qast-is 'sub f(int $a is rw --> int) { $a = $a + 1 }', -> \v {
    qast-contains-op(v, 'decont_i') && !qast-contains-op(v, 'p6decontrv')
}, 'a native assignment body reads the stored value rather than coercing', :full;

qast-is 'sub f(int $a --> int) { $a.abs }', -> \v {
    qast-contains-op(v, 'unbox_i')
}, 'an object body is coerced to a native int return', :full;

sub infix:<add>(int $a, int $b --> int) { $a + $b }
ok nqp::isconcrete(nqp::getattr(&infix:<add>, Routine, '$!inline_info')),
    'a native operator with a native body records inline info';

qast-is 'sub infix:<add>(int $a, int $b --> int) { $a + $b }; my int $i = 1; my $r = $i add 2', -> \v {
    !qast-contains-call(v, '&infix:<add>') && qast-contains-op(v, 'add_i')
}, 'a call to a native operator with a native body is spliced in place', :full;

# vim: expandtab shiftwidth=4
