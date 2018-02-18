use lib <t/packages/>;
use Test;
use Test::Helpers;

plan 1;

{ # https://github.com/rakudo/rakudo/issues/1534
    (temp %*ENV)<MVM_SPESH_BLOCKING  MVM_SPESH_NODELAY> = 1, 1;
    is-run ｢use Test; use Test; print "pass"｣, :out<pass>,
        'no SPESH crashes with duplicate `use Test`';
}

# vim: expandtab shiftwidth=4 ft=perl6
