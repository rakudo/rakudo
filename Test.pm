# Copyright (C) 2007, The Perl Foundation.
# $Id$

## This is a temporary Test.pm to get us started until we get pugs's Test.pm
## working. It's shamelessly stolen & adapted from MiniPerl6 in the pugs repo.

# globals to keep track of our tests
my $num_of_tests_run = 0;
my $num_of_tests_failed = 0;
my $num_of_tests_planned;

# for running the test suite multiple times in the same process
my $testing_started;


## test functions

sub plan($number_of_tests) {
    $testing_started      = 1;
    $num_of_tests_planned = $number_of_tests;

    say '1..' ~ $number_of_tests;
}


sub ok($cond, $desc) {
    proclaim($cond, 'ok! ' ~ $desc);
}


sub is($got, $expected, $desc) {
    my $test = $got eq $expected;
    proclaim($test, 'is! ' ~ $desc);
}


sub isnt($got, $expected, $desc) {
    my $test = !($got eq $expected);
    proclaim($test, 'isnt! ' ~ $desc);
}


## 'private' subs

sub proclaim($cond, $desc) {
    $testing_started  = 1;
    $num_of_tests_run = $num_of_tests_run + 1;

    if ( $cond ) {
        say "ok ", $num_of_tests_run;
    } else {
        say "not ok ", $num_of_tests_run;
    }
}
