use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

plan 5;

# https://github.com/rakudo/rakudo/issues/1534
{
    (temp %*ENV)<MVM_SPESH_BLOCKING  MVM_SPESH_NODELAY> = 1, 1;
    is-run ｢use Test; use Test; print "pass"｣, :out<pass>, :compiler-args[<-I lib>],
        'no SPESH crashes with duplicate `use Test`';
}

lives-ok { class C { }; await start { for ^10_0000 { C.^set_name('B') } } xx 4 },
    'No SEGV when many threads try to change the debug type name';

# https://github.com/rakudo/rakudo/issues/3469
{
    use nqp;
    nqp::srand(1);
    my @first  := nqp::rand_I(100,Int), nqp::rand_I(100,Int);
    nqp::srand(1);
    my @second := nqp::rand_I(100,Int), nqp::rand_I(100,Int);
    is-deeply @second, @first, 'does srand produce same rand_I values';

    nqp::srand(1);
    @first  := nqp::rand_n(100e0), nqp::rand_n(100e0);
    nqp::srand(1);
    @second := nqp::rand_n(100e0), nqp::rand_n(100e0);
    is-deeply @second, @first, 'does srand produce same rand_n values';
}

lives-ok
    { my $l = Lock.new; for ^100_000 { try $l.clone } },
    'Repeatedly trying to clone a Lock does not lead to a crash';

# vim: expandtab shiftwidth=4
