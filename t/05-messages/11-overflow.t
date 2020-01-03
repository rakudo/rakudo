use lib <t/packages/>;
use Test;
use Test::Helpers;

# This file contains tests for behaviour on overflow in various routines

plan 2;

# RT #125820
subtest '.roll' => {
    plan 3;
    
    throws-like { <a b c d e>.roll(-9999999999999999999999999999999999999999999999999).raku },
        Exception, :message{ .contains: <unbox native>.all }, '(1)';

    throws-like { <a b c d e>.roll(-99999999999999999999999999999999999999999999999999999999999999999).raku },
        Exception, :message{ .contains: <unbox native>.all }, '(2)';

    throws-like { <a b c d e>.roll(99999999999999999999999999999999999999999999999999999999999999999).raku },
        Exception, :message{ .contains: <unbox native>.all }, '(3)';
}

# RT #125816
subtest '.indent' => {
    plan 6;

    throws-like { "x".indent(999999999999999999999999999999999) },
        Exception, :message{ .contains: <unbox native>.all }, '(1)';

    throws-like { "x".indent(9999999999999999999999999999999999999999999999999) },
        Exception, :message{ .contains: <unbox native>.all }, '(2)';

    throws-like { "x".indent(9999999999999999999999999999999999999999999999999999999999999999999999999) },
        Exception, :message{ .contains: <unbox native>.all }, '(3)';

    quietly {
        throws-like { "x".indent(-999999999999999999999999999999999) },
            Exception, :message{ .contains: <unbox native>.all }, '(4)';

        throws-like { "x".indent(-9999999999999999999999999999999999999999999999999) },
            Exception, :message{ .contains: <unbox native>.all }, '(5)';

        throws-like { "x".indent(-9999999999999999999999999999999999999999999999999999999999999999999999999) },
            Exception, :message{ .contains: <unbox native>.all }, '(6)';
    }
}

# vim: ft=perl6 expandtab sw=4
