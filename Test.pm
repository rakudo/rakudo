# Copyright (C) 2007, The Perl Foundation.
# $Id$

## This is a temporary Test.pm to get us started until we get pugs's Test.pm
## working. It's shamelessly stolen & adapted from MiniPerl6 in the pugs repo.

# globals to keep track of our tests
our $num_of_tests_run = 0;
our $num_of_tests_failed = 0;
our $num_of_tests_planned;
our $todo_upto_test_num = 0;
our $todo_reason = '';

# for running the test suite multiple times in the same process
our $testing_started;


## test functions

# Compare numeric values with approximation
sub approx ($x, $y) {
    my $epsilon = 0.00001;
    my $diff = abs($x - $y);
    ($diff < $epsilon);
}

sub plan($number_of_tests) {
    $testing_started      = 1;
    $num_of_tests_planned = $number_of_tests;

    say '1..' ~ $number_of_tests;
}


multi sub ok($cond, $desc) {
    proclaim($cond, $desc);
}

multi sub ok($cond) { ok($cond, ''); }


multi sub is($got, $expected, $desc) {
    my $test = $got eq $expected;
    proclaim($test, $desc);
}

multi sub is($got, $expected) { is($got, $expected, ''); }


multi sub isnt($got, $expected, $desc) {
    my $test = !($got eq $expected);
    proclaim($test, $desc);
}

multi sub isnt($got, $expected) { isnt($got, $expected, ''); }

multi sub is_approx($got, $expected, $desc) {
    my $test = abs($got - $expected) <= 0.00001;
    proclaim($test, $desc);
}

multi sub is_approx($got, $expected) { is_approx($got, $expected, ''); }

multi sub todo($reason, $count) {
    $todo_upto_test_num = $num_of_tests_run + $count;
    $todo_reason = 'TODO ' ~ $reason;
}

multi sub skip()                { proclaim(1, "skip "); }
multi sub skip($reason)         { proclaim(1, "skip $reason"); }
multi sub skip($count, $reason) { skip($reason); }

multi sub skip_rest() {
    skip($num_of_tests_planned - $num_of_tests_run, "");
}

multi sub skip_rest($reason) {
    skip($num_of_tests_planned - $num_of_tests_run, $reason);
}

sub diag($message) { say '# '~$message; }

## 'private' subs

sub proclaim($cond, $desc) {
    $testing_started  = 1;
    $desc = $todo_reason
        if $todo_reason and $num_of_tests_run < $todo_upto_test_num;
    $num_of_tests_run = $num_of_tests_run + 1;

    if ( $cond ) {
        print "ok " ~ $num_of_tests_run ~ " - ";
    } else {
        print "not ok " ~ $num_of_tests_run ~ " - ";
        ++$num_of_tests_failed
            unless  $num_of_tests_run <= $todo_upto_test_num;
    }
    say $desc;

}

END {
    # until END blocks can access compile-time symbol tables of outer scopes,
    #  we need these declarations
    our $testing_started;
    our $num_of_tests_planned;
    our $num_of_tests_run;
    our $num_of_tests_failed;

    if ($testing_started and $num_of_tests_planned != $num_of_tests_run) {  ##Wrong quantity of tests
        diag("Looks like you planned $num_of_tests_planned tests, but ran $num_of_tests_run");
    }
    if ($testing_started and $num_of_tests_failed) {
        diag("Looks like you failed $num_of_tests_failed tests of $num_of_tests_run");
    }
}
