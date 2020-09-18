# Sanity test to make sure the first 100000 prime numbers are correct
# as these have been pre-calculated in the core

use nqp;

for -99999 .. 99999 {
    if nqp::isprime_I($_,20) != .is-prime {
        say "not ok 1 - $_ mismatches prime test";
        exit
    }
}

say "ok 1 - prime test for first 100000 values correct";
END say "1..1";
