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


multi sub nok($cond, $desc) {
    proclaim(!$cond, $desc);
}

multi sub nok($cond) { nok(!$cond, ''); }


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
    $todo_reason = '# TODO ' ~ $reason;
}

multi sub todo($reason) {
    $todo_upto_test_num = $num_of_tests_run + 1;
    $todo_reason = '# TODO ' ~ $reason;
}

multi sub skip()                { proclaim(1, "# SKIP"); }
multi sub skip($reason)         { proclaim(1, "# SKIP " ~ $reason); }
multi sub skip($count, $reason) {
    for 1..$count {
        proclaim(1, "# SKIP " ~ $reason);
    }
}

multi sub skip_rest() {
    skip($num_of_tests_planned - $num_of_tests_run, "");
}

multi sub skip_rest($reason) {
    skip($num_of_tests_planned - $num_of_tests_run, $reason);
}

sub diag($message) { say '# '~$message; }


multi sub flunk($reason) { proclaim(0, "flunk $reason")}


sub isa_ok($var,$type) { ok($var.isa($type), "The object is-a '$type'"); }

multi sub dies_ok($closure, $reason) {
    try {
        $closure();
    }
    proclaim((defined $!), $reason);
}
multi sub dies_ok($closure) {
    dies_ok($closure, '');
}

multi sub lives_ok($closure, $reason) {
    try {
        $closure();
    }
    proclaim((not defined $!), $reason);
}
multi sub lives_ok($closure) {
    lives_ok($closure, '');
}

multi sub eval_dies_ok($code, $reason) {
    proclaim((defined eval_exception($code)), $reason);
}
multi sub eval_dies_ok($code) {
    eval_dies_ok($code, '');
}

multi sub eval_lives_ok($code, $reason) {
    proclaim((not defined eval_exception($code)), $reason);
}
multi sub eval_lives_ok($code) {
    eval_lives_ok($code, '');
}


## 'private' subs

sub eval_exception($code) {
    my $eval_exception;
    try { eval ($code); $eval_exception = $! }
    $eval_exception // $!;
}

sub proclaim($cond, $desc) {
    $testing_started  = 1;
    $num_of_tests_run = $num_of_tests_run + 1;

    unless $cond {
        print "not ";
        $num_of_tests_failed = $num_of_tests_failed + 1
            unless  $num_of_tests_run <= $todo_upto_test_num;
    }
    print "ok ", $num_of_tests_run, " - ", $desc;
    if $todo_reason and $num_of_tests_run <= $todo_upto_test_num {
        print $todo_reason;
    }
    print "\n";
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
