module Test;
# Copyright (C) 2007 - 2011 The Perl Foundation.

## This is a temporary Test.pm to get us started until we get pugs's Test.pm
## working. It's shamelessly stolen & adapted from MiniPerl6 in the pugs repo.

# variables to keep track of our tests
my $num_of_tests_run    = 0;
my $num_of_tests_failed = 0;
my $todo_upto_test_num  = 0;
my $todo_reason         = '';
my $num_of_tests_planned;
my $no_plan = 1;
my $die_on_fail;
my $perl6_test_times = ? %*ENV<PERL6_TEST_TIMES>;
my $time_before;
my $time_after;

## If done_testing hasn't been run when we hit our END block, we need to know
## so that it can be run. This allows compatibility with old tests that use
## plans and don't call done_testing.
my $done_testing_has_been_run = 0;


## test functions

# you can call die_on_fail; to turn it on and die_on_fail(0) to turn it off
sub die_on_fail($fail=1) {
    $die_on_fail = $fail;
}

# "plan 'no_plan';" is now "plan *;"
# It is also the default if nobody calls plan at all
multi sub plan($number_of_tests) is export {
    if $number_of_tests ~~ ::Whatever {
        $no_plan = 1;
    }
    else {
        $num_of_tests_planned = $number_of_tests;
        $no_plan = 0;

        say '1..' ~ $number_of_tests;
    }
    # Get two successive timestamps to say how long it takes to read the
    # clock, and to let the first test timing work just like the rest.
    # These readings should be made with the expression now.to-posix[0],
    # but its execution time when tried in the following two lines is a
    # lot slower than the non portable nqp::p6box_n(pir::time__N).
    $time_before = nqp::p6box_n(pir::time__N);
    $time_after  = nqp::p6box_n(pir::time__N);
    say '# between two timestamps ' ~
        ceiling(($time_after-$time_before)*1_000_000) ~ ' microseconds'
        if $perl6_test_times;
    # Take one more reading to serve as the begin time of the first test
    $time_before = nqp::p6box_n(pir::time__N);
}

multi sub pass($desc = '') is export {
    $time_after = nqp::p6box_n(pir::time__N);
    proclaim(1, $desc);
    $time_before = nqp::p6box_n(pir::time__N);
}

multi sub ok(Mu $cond, $desc = '') is export {
    $time_after = nqp::p6box_n(pir::time__N);
    proclaim(?$cond, $desc);
    $time_before = nqp::p6box_n(pir::time__N);
}

multi sub nok(Mu $cond, $desc = '') is export {
    $time_after = nqp::p6box_n(pir::time__N);
    proclaim(!$cond, $desc);
    $time_before = nqp::p6box_n(pir::time__N);
}

multi sub is(Mu $got, Mu $expected, $desc = '') is export {
    $time_after = nqp::p6box_n(pir::time__N);
    $got.defined; # Hack to deal with Failures
    my $test = $got eq $expected;
    proclaim(?$test, $desc);
    if !$test {
        diag "     got: '$got'";
        diag "expected: '$expected'";
    }
    $test;
    $time_before = nqp::p6box_n(pir::time__N);
}

multi sub isnt(Mu $got, Mu $expected, $desc = '') is export {
    $time_after = nqp::p6box_n(pir::time__N);
    my $test = !($got eq $expected);
    proclaim($test, $desc);
    $time_before = nqp::p6box_n(pir::time__N);
}

multi sub is_approx(Mu $got, Mu $expected, $desc = '') is export {
    $time_after = nqp::p6box_n(pir::time__N);
    my $tol = $expected.abs < 1e-6 ?? 1e-5 !! $expected.abs * 1e-6;
    my $test = ($got - $expected).abs <= $tol;
    proclaim(?$test, $desc);
    unless $test {
        diag("got:      $got");
        diag("expected: $expected");
    }
    ?$test;
    $time_before = nqp::p6box_n(pir::time__N);
}

multi sub todo($reason, $count = 1) is export {
    $time_after = nqp::p6box_n(pir::time__N);
    $todo_upto_test_num = $num_of_tests_run + $count;
    $todo_reason = '# TODO ' ~ $reason;
    $time_before = nqp::p6box_n(pir::time__N);
}

multi sub skip() {
    $time_after = nqp::p6box_n(pir::time__N);
    proclaim(1, "# SKIP");
    $time_before = nqp::p6box_n(pir::time__N);
}
multi sub skip($reason, $count = 1) is export {
    $time_after = nqp::p6box_n(pir::time__N);
    die "skip() was passed a non-numeric number of tests.  Did you get the arguments backwards?" if $count !~~ Numeric;
    my $i = 1;
    while $i <= $count { proclaim(1, "# SKIP " ~ $reason); $i = $i + 1; }
    $time_before = nqp::p6box_n(pir::time__N);
}

sub skip_rest($reason = '<unknown>') is export {
    $time_after = nqp::p6box_n(pir::time__N);
    skip($reason, $num_of_tests_planned - $num_of_tests_run);
    $time_before = nqp::p6box_n(pir::time__N);
}

sub diag($message) is export {
    $time_after = nqp::p6box_n(pir::time__N);
    say $message.Str.subst(rx/^^/, '# ', :g);
    $time_before = nqp::p6box_n(pir::time__N);
}

multi sub flunk($reason) is export {
    $time_after = nqp::p6box_n(pir::time__N);
    proclaim(0, "flunk $reason");
    $time_before = nqp::p6box_n(pir::time__N);
}

multi sub isa_ok(Mu $var, Mu $type, $msg = ("The object is-a '" ~ $type.perl ~ "'")) is export {
    $time_after = nqp::p6box_n(pir::time__N);
    ok($var.isa($type), $msg)
        or diag('Actual type: ' ~ $var.WHAT);
    $time_before = nqp::p6box_n(pir::time__N);
}

multi sub dies_ok(Callable $closure, $reason = '') is export {
    $time_after = nqp::p6box_n(pir::time__N);
    my $death = 1;
    my $bad_death = 0;
    try {
        $closure();
        $death = 0;
    }
    if $death && $!.Str.index('Null PMC access') {
        $bad_death = 1;
        diag("Wrong way to die: '$!'");
    }
    proclaim( $death && !$bad_death, $reason );
    $time_before = nqp::p6box_n(pir::time__N);
}

multi sub lives_ok(Callable $closure, $reason = '') is export {
    $time_after = nqp::p6box_n(pir::time__N);
    try {
        $closure();
    }
    proclaim((not defined $!), $reason) or diag($!);
    $time_before = nqp::p6box_n(pir::time__N);
}

multi sub eval_dies_ok(Str $code, $reason = '') is export {
    $time_after = nqp::p6box_n(pir::time__N);
    my $ee = eval_exception($code);
    if defined $ee {
        # XXX no regexes yet in nom
        my $bad_death = $ee.Str.index('Null PMC access ').defined;
        if $bad_death {
            diag "wrong way to die: '$ee'";
        }
        proclaim( !$bad_death, $reason );
    }
    else {
        proclaim( 0, $reason );
    }
    $time_before = nqp::p6box_n(pir::time__N);
}

multi sub eval_lives_ok(Str $code, $reason = '') is export {
    $time_after = nqp::p6box_n(pir::time__N);
    my $ee = eval_exception($code);
    proclaim((not defined $ee), $reason)
        or diag("Error: $ee");
    $time_before = nqp::p6box_n(pir::time__N);
}

multi sub is_deeply(Mu $got, Mu $expected, $reason = '') is export
{
    $time_after = nqp::p6box_n(pir::time__N);
    my $test = _is_deeply( $got, $expected );
    proclaim($test, $reason);
    if !$test {
        my $got_perl      = try { $got.perl };
        my $expected_perl = try { $expected.perl };
        if $got_perl.defined && $expected_perl.defined {
            diag "     got: $got_perl";
            diag "expected: $expected_perl";
        }
    }
    $test;
    $time_before = nqp::p6box_n(pir::time__N);
}

sub _is_deeply(Mu $got, Mu $expected) {
    $got eqv $expected;
}


## 'private' subs

sub eval_exception($code) {
    try {
        eval ($code);
    }
    $!;
}

sub proclaim($cond, $desc) {
    # exclude the time spent in proclaim from the test time
    $num_of_tests_run = $num_of_tests_run + 1;

    unless $cond {
        print "not ";
        unless  $num_of_tests_run <= $todo_upto_test_num {
            $num_of_tests_failed = $num_of_tests_failed + 1
        }
    }
    print "ok ", $num_of_tests_run, " - ", $desc;
    if $todo_reason and $num_of_tests_run <= $todo_upto_test_num {
        print $todo_reason;
    }
    print "\n";
    print "# t=" ~ ceiling(($time_after-$time_before)*1_000_000) ~ "\n"
        if $perl6_test_times;

    if !$cond && $die_on_fail && !$todo_reason {
        die "Test failed.  Stopping test";
    }
    # must clear this between tests
    if $todo_upto_test_num == $num_of_tests_run { $todo_reason = '' }
    $cond;
}

sub done_testing() is export {
    die "done_testing() has been renamed to done(), please change your test code";
}

sub done() is export {
    $done_testing_has_been_run = 1;

    if $no_plan {
        $num_of_tests_planned = $num_of_tests_run;
        say "1..$num_of_tests_planned";
    }

    if ($num_of_tests_planned != $num_of_tests_run) {  ##Wrong quantity of tests
        diag("Looks like you planned $num_of_tests_planned tests, but ran $num_of_tests_run");
    }
    if ($num_of_tests_failed) {
        diag("Looks like you failed $num_of_tests_failed tests of $num_of_tests_run");
    }
}

END {
    ## In planned mode, people don't necessarily expect to have to call done
    ## So call it for them if they didn't
    if !$done_testing_has_been_run && !$no_plan {
        done;
    }
}

# vim: ft=perl6
