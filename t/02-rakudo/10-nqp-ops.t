use lib <t/packages/>;
use Test;
use Test::Helpers;
use nqp;
# Tests for nqp ops that don't fit into nqp's test suit

plan 2;

# https://github.com/Raku/old-issue-tracker/issues/6538
lives-ok {
    nqp::p6bindattrinvres(($ := 42), Int, q|$!value|, nqp::getattr(42, Int, q|$!value|))
}, 'p6bindattrinvres with getattr of bigint does not crash';

# https://github.com/Raku/old-issue-tracker/issues/6614
is-run ｢use nqp; quietly print nqp::getlexdyn('&DEPRECATED'); print 'pass'｣,
    :out<pass>, 'getlexdyn op does not segfault';

# vim: expandtab shiftwidth=4
