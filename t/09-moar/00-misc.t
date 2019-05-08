use lib <t/packages/>;
use Test;
use Test::Helpers;

plan 2;

{ # https://github.com/rakudo/rakudo/issues/1534
    (temp %*ENV)<MVM_SPESH_BLOCKING  MVM_SPESH_NODELAY> = 1, 1;
    is-run ｢use Test; use Test; print "pass"｣, :out<pass>,
        'no SPESH crashes with duplicate `use Test`';
}

lives-ok { class C { }; await start { for ^10_0000 { C.^set_name('B') } } xx 4 },
    'No SEGV when many threads try to change the debug type name';

# vim: expandtab shiftwidth=4 ft=perl6
