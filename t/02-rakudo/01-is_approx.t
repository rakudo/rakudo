use v6;
use lib 'lib';
use Test;

plan 11;

# These tests are meant to test now-deprecated `is_approx`, not `is-approx`
# If `is_approx` is removed entirely, remove this test file. The `is-approx`
# routine is already tested in roast.
%*ENV<RAKUDO_NO_DEPRECATIONS> = 1;

# "large" numbers
{
    my $speed_of_light = 2.99792458e8;
    my $not_quite_sol = 2.997925e8;

    # expect to pass with current implementation
    is_approx($not_quite_sol, $speed_of_light,
        "approx within 1e-5");
    # however is not "within" 1e-5 but differ by 42

    # expect to fail with current implementation
    $not_quite_sol = 2.99793e8;
    my $message = "should fail; approx *not* within 1e-5";
    todo $message;
    my $ok = is_approx($not_quite_sol, $speed_of_light, $message);
    # however is not "within" 1e-5 but differ by 542
    nok($ok);
}

# "normal" numbers
{
    my $eulers_constant = 2.71828182;
    my $not_quite_ec = 2.71828;

    # expect to pass with current implementation
    is_approx($not_quite_ec, $eulers_constant,
        "approx within 1e-5");

    # expect to fail with current implementation
    $not_quite_ec = 2.71829;
    my $message = "should fail; approx *not* within 1e-5";
    todo $message;
    my $ok = is_approx($not_quite_ec, $eulers_constant, $message);
    nok($ok, $message);
}

# "small" numbers
{
    my $exp = 6.62609657;

    # expect to pass with current implementation
    is_approx($exp - 1e-6, $exp,
        "should pass; approx within 1e-5");

    # expect to fail with current implementation
    my $message = "should fail; approx *not* within 1e-5 (1)";
    todo $message;
    my $ok = is_approx($exp - 2e-5, $exp, $message);
    nok($ok, $message);
    # however passes, since numbers themselves are smaller than 1e-5

    # *really* expect to fail with current implementation
    $message = "should fail; approx *not* within 1e-5 (2)";
    todo $message;
    $ok = is_approx($exp + 42, $exp, $message);
    nok($ok, $message);
    # however passes, since numbers themselves are smaller than 1e-5
}

# vim: expandtab shiftwidth=4
