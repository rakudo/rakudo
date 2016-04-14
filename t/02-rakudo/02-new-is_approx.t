use v6;
use lib 'lib';
use Test;

plan 7;

# "large" numbers
subtest {
    my $speed_of_light = 2.99792458e8;
    my $not_quite_sol = 2.997925e8;

    # expect to pass: within precision tolerance
    is-approx($not_quite_sol, $speed_of_light,
        "approx within precision");

    # expect to fail: *not* within precision tolerance
    $not_quite_sol = 2.99793e8;
    my $message = "should fail; approx *not* within precision";
    todo $message;
    my $ok = is-approx($not_quite_sol, $speed_of_light, $message);
    nok($ok, $message);
}, "check behaviour of 'large' numbers";

# "normal" numbers; also test subtest(Desc, Code) multi
subtest "check behaviour of 'normal' numbers", {
    my $eulers_constant = 2.71828182;
    my $not_quite_ec = 2.71828;

    # expect to pass: within precision tolerance
    is-approx($not_quite_ec, $eulers_constant,
        "approx within precision");

    # expect to fail: *not* within precision tolerance
    $not_quite_ec = 2.71829;
    my $message = "should fail; approx *not* within precision";
    todo $message;
    my $ok = is-approx($not_quite_ec, $eulers_constant, $message);
    nok($ok, $message);
};

# "small" numbers; also test subtest(Pair[desc,code]) multi
subtest "check behaviour of 'small' numbers" => {
    my $plancks_constant = 6.62609657e-34;
    my $not_quite_pc = 6.62609e-34;

    # expect to pass: within precision tolerance
    is-approx($not_quite_pc, $plancks_constant,
        "should pass; within precision");

    # expect to fail: *not* within precision tolerance
    $not_quite_pc = 6.62608e-34;
    my $message = "should fail; approx *not* within precision";
    todo $message;
    my $ok = is-approx($not_quite_pc, $plancks_constant, $message);
    nok($ok, $message);

    # *really* expect to fail: *not* within precision tolerance
    $message = "should fail; approx *really* *not* within precision";
    $not_quite_pc = 16.62608e-34;
    todo $message;
    $ok = is-approx($not_quite_pc, $plancks_constant, $message);
    nok($ok, $message);

    # expect to pass within specified precision
    $not_quite_pc = 6.62608e-34;
    is-approx($not_quite_pc, $plancks_constant, 1e-5,
        "should pass; approx within explicit precision");

    # expect to fail; *not* within specified precision
    $message = "should fail; approx *not* within explicit precision";
    $not_quite_pc = 6.62608e-34;
    todo $message;
    $ok = is-approx($not_quite_pc, $plancks_constant, 1e-7, $message);
    nok($ok, $message);
};

# check tolerance input
subtest {
    my $message = "should fail; cannot use negative tolerance values";
    dies-ok { is-approx(1, 1, -1, $message) }, $message;

    $message = "should fail; cannot use a zero tolerance value";
    dies-ok { is-approx(1, 1, 0, $message) }, $message;
}, "check tolerance input";

# expected value is zero
subtest {
    is-approx(0.000001, 0, abs_tol => 1e-6, desc => "approximately zero");
    is-approx(0.000001, 0, rel_tol => 1e-6, abs_tol => 1e-6,
              desc => "approximately zero");

    my $message = "should fail; not approximately zero";
    todo $message;
    my $ok = is-approx(0.00001, 0, abs_tol => 1e-6, desc => $message);
    nok($ok, $message);
}, "check behaviour when input values are around zero";

# symmetry
subtest {
    my $pi = 3.14159265358979;
    my $almost_pi = 3.141592;
    my $ok_p_ap = is-approx($pi, $almost_pi, "pi is approximately almost_pi");
    my $ok_ap_p = is-approx($almost_pi, $pi, "almost_pi is approximately pi");
    ok($ok_p_ap && $ok_ap_p, "is-approx is symmetric under argument change");
}, "check symmetry with respect to argument swapping";

# check behaviour of equal values
subtest {
    my $pi = 3.14159265358979;
    is-approx($pi, 3.14159265358979, "equal values around one order of magnitude");
}, "check behaviour of equal values";

# vim: expandtab shiftwidth=4 ft=perl6
