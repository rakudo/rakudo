use v6;
use lib 'lib';
use Test;


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
    is_approx($not_quite_sol, $speed_of_light,
        "should fail; approx *not* within 1e-5");
    # however is not "within" 1e-5 but differ by 542
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
    is_approx($not_quite_ec, $eulers_constant,
        "should fail; approx *not* within 1e-5");
}

# "small" numbers
{
    my $plancks_constant = 6.62609657e-34;
    my $not_quite_pc = 6.62609e-34;

    # expect to pass with current implementation
    is_approx($not_quite_pc, $plancks_constant,
        "should pass; approx within 1e-5");

    # expect to fail with current implementation
    $not_quite_pc = 6.62608e-34;
    is_approx($not_quite_pc, $plancks_constant,
        "should fail; approx *not* within 1e-5");
    # however passes, since numbers themselves are smaller than 1e-5

    # *really* expect to fail with current implementation
    $not_quite_pc = 16.62608e-34;
    is_approx($not_quite_pc, $plancks_constant,
        "should fail; approx *not* within 1e-5");
    # however passes, since numbers themselves are smaller than 1e-5
}

done;


# vim: expandtab shiftwidth=4 ft=perl6
