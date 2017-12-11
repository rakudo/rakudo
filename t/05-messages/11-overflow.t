use lib <t/packages/>;
use Test;
use Test::Helpers;

# This file contains tests for behaviour on overflow in various routines

plan 1;

# RT #125820
subtest '.roll' => {
    plan 3;
    
    throws-like { <a b c d e>.roll(-9999999999999999999999999999999999999999999999999).perl },
        Exception, :message{ .contains: <unbox native>.all }, '(1)';

    throws-like { <a b c d e>.roll(-99999999999999999999999999999999999999999999999999999999999999999).perl },
        Exception, :message{ .contains: <unbox native>.all }, '(2)';

    throws-like { <a b c d e>.roll(99999999999999999999999999999999999999999999999999999999999999999).perl },
        Exception, :message{ .contains: <unbox native>.all }, '(3)';
}

# vim: ft=perl6 expandtab sw=4
