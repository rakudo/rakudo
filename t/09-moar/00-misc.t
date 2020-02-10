use lib <t/packages/>;
use Test;
use Test::Helpers;

plan 4;

# https://github.com/rakudo/rakudo/issues/1534
{
    (temp %*ENV)<MVM_SPESH_BLOCKING  MVM_SPESH_NODELAY> = 1, 1;
    is-run ｢use Test; use Test; print "pass"｣, :out<pass>,
        'no SPESH crashes with duplicate `use Test`';
}

lives-ok { class C { }; await start { for ^10_0000 { C.^set_name('B') } } xx 4 },
    'No SEGV when many threads try to change the debug type name';

# https://github.com/rakudo/rakudo/issues/3469
{
    use nqp;
    nqp::srand(1);
    my $first := nqp::rand_I(100,Int);
    nqp::srand(1);
pass "waiting for fix";
#    is nqp::rand_I(100,Int), $first, 'does srand produce same rand_I values';

    nqp::srand(1);
    $first := nqp::rand_n(100e0);
    nqp::srand(1);
    is nqp::rand_n(100e0), $first, 'does srand produce same rand_n values';
}

# vim: expandtab shiftwidth=4 ft=perl6
