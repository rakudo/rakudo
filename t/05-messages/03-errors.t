use lib <t/packages/>;
use Test;
use Test::Helpers;

plan 1;

subtest '.map does not explode in optimizer' => {
    plan 3;
    throws-like ｢^4 .map: {}｣, Exception,
        :message{.contains: 'Cannot map a Range to a Hash.'}, 'hash';
    throws-like ｢^4 .map: 42｣, X::Multi::NoMatch, 'Int';

    sub foo ($x) { $x+2};
    is-deeply ^4 .map(&foo), (2, 3, 4, 5).Seq, 'subroutine';
}

# vim: ft=perl6 expandtab sw=4
