use lib <t/packages/>;
use Test;
use Test::Helpers;
use nqp;
# Tests for nqp ops that don't fit into nqp's test suit

plan 2;

# RT#132126
lives-ok {
    nqp::p6bindattrinvres(($ := 42), Int, q|$!value|, nqp::getattr(42, Int, q|$!value|))
}, 'p6bindattrinvres with getattr of bigint does not crash';

# RT #132300
is-run ｢use nqp; quietly print nqp::getlexdyn('&DEPRECATED'); print 'pass'｣,
    :out<pass>, 'getlexdyn op does not segfault';
