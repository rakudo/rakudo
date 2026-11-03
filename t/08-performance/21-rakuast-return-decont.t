use lib <t/packages/Test-Helpers>;
use Test::Helpers::QAST;
use Test;
use QAST:from<NQP>;
use nqp;
plan 12;

# A routine whose fall-through result is provably container-free skips the
# return decontainerization, and a native assignment result downgrades it to
# a plain native decont. A result that may carry a container keeps the full
# op, as does everything under --optimize=off.

# These observe the emitted QAST.
qast-is 'sub rd-a() { 42 }', :full, -> \v {
    not qast-contains-op(v, 'p6decontrv') and not qast-contains-op(v, 'p6decontrv_6c')
}, 'a literal result skips the return decont';

qast-is 'sub rd-b() { Int }', :full, -> \v {
    not qast-contains-op(v, 'p6decontrv') and not qast-contains-op(v, 'p6decontrv_6c')
}, 'a type object result skips the return decont';

qast-is 'my int $i = 3; sub rd-c() { $i }', :full, -> \v {
    not qast-contains-op(v, 'p6decontrv') and not qast-contains-op(v, 'p6decontrv_6c')
}, 'a native variable result skips the return decont';

qast-is 'my int $i; sub rd-d() { $i = 5 }', :full, -> \v {
        qast-contains-op(v, 'decont_i')
    and not qast-contains-op(v, 'p6decontrv') and not qast-contains-op(v, 'p6decontrv_6c')
}, 'a native assignment result downgrades to a native decont';

qast-is 'sub rd-e() { my $x = 5; $x }', :full, -> \v {
    qast-contains-op(v, 'p6decontrv') or qast-contains-op(v, 'p6decontrv_6c')
}, 'a boxed variable result keeps the return decont';

qast-is 'sub rd-f() { return 1 }; sub rd-g() { rd-f() }', :full, -> \v {
    qast-contains-op(v, 'p6decontrv') or qast-contains-op(v, 'p6decontrv_6c')
}, 'a call result keeps the return decont';

# These observe that returns still behave.
{
    sub f() { 42 }
    is f(), 42, 'a literal return gives its value';
}
{
    my int $i = 7;
    sub f() { $i }
    is f(), 7, 'a native variable return gives its value';
}
{
    my int $i;
    sub f() { $i = 5 }
    is f(), 5, 'a native assignment return gives the assigned value';
    is $i, 5, 'the native assignment still stores';
}
{
    sub f() { my $x = 5; $x }
    my $r = f();
    $r = 9;
    is f(), 5, 'a returned copy does not alias the inner container';
}
{
    my $x = 5;
    sub f() is rw { $x }
    f() = 9;
    is $x, 9, 'an rw routine still returns its container';
}

# vim: expandtab shiftwidth=4
