use v6;
use lib 'lib';
use Test;

# "large" numbers
{
    my $speed_of_light = 2.99792458e8;
    my $not_quite_sol = 2.997925e8;

    # expect to pass: within precision tolerance
    is_approx($not_quite_sol, $speed_of_light,
        "approx within precision");

    # expect to fail: *not* within precision tolerance
    $not_quite_sol = 2.99793e8;
    is_approx($not_quite_sol, $speed_of_light,
        "should fail; approx *not* within precision");
}

# "normal" numbers
{
    my $eulers_constant = 2.71828182;
    my $not_quite_ec = 2.71828;

    # expect to pass: within precision tolerance
    is_approx($not_quite_ec, $eulers_constant,
        "approx within precision");

    # expect to fail: *not* within precision tolerance
    $not_quite_ec = 2.71829;
    is_approx($not_quite_ec, $eulers_constant,
        "should fail; approx *not* within precision");
}

# "small" numbers
{
    my $plancks_constant = 6.62609657e-34;
    my $not_quite_pc = 6.62609e-34;

    # expect to pass: within precision tolerance
    is_approx($not_quite_pc, $plancks_constant,
        "should pass; within precision");

    # expect to fail: *not* within precision tolerance
    $not_quite_pc = 6.62608e-34;
    is_approx($not_quite_pc, $plancks_constant,
        "should fail; approx *not* within precision");

    # *really* expect to fail: *not* within precision tolerance
    $not_quite_pc = 16.62608e-34;
    is_approx($not_quite_pc, $plancks_constant,
        "should fail; approx *really* *not* within precision");

    # expect to pass within specified precision
    $not_quite_pc = 6.62608e-34;
    is_approx($not_quite_pc, $plancks_constant, 1e-5,
        "should pass; approx within explicit precision");

    # expect to fail; *not* within specified precision
    $not_quite_pc = 6.62608e-34;
    is_approx($not_quite_pc, $plancks_constant, 1e-7,
        "should fail; approx *not* within explicit precision");
}

done;


# vim: expandtab shiftwidth=4 ft=perl6
