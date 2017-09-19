use Test;
use nqp;
# Tests for nqp ops that don't fit into nqp's test suit

plan 1;

lives-ok {
    nqp::p6bindattrinvres(($ := 42), Int, q|$!value|, nqp::getattr(42, Int, q|$!value|))
}, 'p6bindattrinvres with getattr of bigint does not crash';
