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
my $time_before = 0E0;
my $time_after  = 0E0;

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
# multi sub plan(Whatever $plan) is export {
#     $no_plan = 1;
# }

proto sub plan(|$) is export { * }
multi sub plan($number_of_tests) {
    if $number_of_tests ~~ ::Whatever {
        $no_plan = 1;
    }
    else {
        $num_of_tests_planned = $number_of_tests;
        $no_plan = 0;

        say '1..' ~ $number_of_tests;
    }
    # Get two successive timestamps to measure the measurement overhead,
    # and to reduce bias, if it exists, from the first test time.
    $time_before = nqp::p6box_n(pir::time__N);
    $time_after  = nqp::p6box_n(pir::time__N);
    say '# between two timestamps ' ~
        ceiling(($time_after-$time_before)*1_000_000) ~ ' microseconds'
        if $perl6_test_times;
    $time_before = nqp::p6box_n(pir::time__N);
    # Ideally the time readings above could be made with the expression
    # now.to-posix[0], but the execution time showed by the difference
    # between the two successive readings is far slower than when the
    # non portable pir::time__N is used instead.
}

proto sub pass(|$) is export { * }
multi sub pass($desc) {
    proclaim(1, $desc);
}

proto sub ok(|$) is export { * }
multi sub ok(Mu $cond, $desc) {
    proclaim(?$cond, $desc);
}
multi sub ok(Mu $cond) { ok(?$cond, ''); }

proto sub nok(|$) is export { * }
multi sub nok(Mu $cond, $desc) {
    proclaim(!$cond, $desc);
}
multi sub nok(Mu $cond) { nok($cond, ''); }

proto sub is(|$) is export { * }
multi sub is(Mu $got, Mu $expected, $desc) {
    $got.defined; # Hack to deal with Failures
    my $test = $got eq $expected;
    proclaim(?$test, $desc);
    if !$test {
        diag "     got: '$got'";
        diag "expected: '$expected'";
    }
    $test;
}
multi sub is(Mu $got, Mu $expected) { is($got, $expected, ''); }

proto sub isnt(|$) is export { * }
multi sub isnt(Mu $got, Mu $expected, $desc) {
    my $test = !($got eq $expected);
    proclaim($test, $desc);
}
multi sub isnt(Mu $got, Mu $expected) { isnt($got, $expected, ''); }

proto sub is_approx(|$) is export { * }
multi sub is_approx(Mu $got, Mu $expected, $desc) {
    my $test = ($got - $expected).abs <= 1/100000;
    proclaim(?$test, $desc);
    unless $test {
        diag("got:      $got");
        diag("expected: $expected");
    }
    ?$test;
}
multi sub is_approx(Mu $got, Mu $expected) {
    is_approx($got, $expected, '');
}

proto sub todo(|$) is export { * }
multi sub todo($reason, $count) {
    $todo_upto_test_num = $num_of_tests_run + $count;
    $todo_reason = '# TODO ' ~ $reason;
}
multi sub todo($reason) {
    $todo_upto_test_num = $num_of_tests_run + 1;
    $todo_reason = '# TODO ' ~ $reason;
}

proto sub skip(|$) is export { * }
multi sub skip()                { proclaim(1, "# SKIP"); }
multi sub skip($reason)         { proclaim(1, "# SKIP " ~ $reason); }
multi sub skip($reason, $count) {
    die "skip() was passed a non-numeric number of tests.  Did you get the arguments backwards?" if $count !~~ Numeric;
    my $i = 1;
    while $i <= $count { proclaim(1, "# SKIP " ~ $reason); $i = $i + 1; }
}

sub skip_rest($reason = '<unknown>') is export {
    skip($reason, $num_of_tests_planned - $num_of_tests_run);
}

sub diag($message) is export {
    # XXX No regexes yet in nom
    #say $message.subst(rx/^^/, '# ', :g);
    say "# " ~ $message;
}

proto sub flunk(|$) is export { * }
multi sub flunk($reason) { proclaim(0, "flunk $reason")}

proto sub isa_ok(|$) is export { * }
multi sub isa_ok(Mu $var, Mu $type) {
    ok($var.isa($type), "The object is-a '" ~ $type.perl ~ "'")
        or diag('Actual type: ' ~ $var.WHAT);
}
multi sub isa_ok(Mu $var, Mu $type, $msg) {
    ok($var.isa($type), $msg)
        or diag('Actual type: ' ~ $var.WHAT);
}

proto sub dies_ok(|$) is export { * }
multi sub dies_ok(Callable $closure, $reason) {
    my $death = 1;
    my $bad_death = 0;
    try {
        $closure();
        $death = 0;
        # XXX no regexes, catch yet in nom
        #CATCH {
            #when / ^ 'Null PMC access ' / {
            #    diag "wrong way to die: '$!'";
            #    $bad_death = 1;
            #}
        #}
    }
    proclaim( $death && !$bad_death, $reason );
}
multi sub dies_ok(Callable $closure) {
    dies_ok($closure, '');
}

proto sub lives_ok(|$) is export { * }
multi sub lives_ok(Callable $closure, $reason){
    try {
        $closure();
    }
    proclaim((not defined $!), $reason);
}
multi sub lives_ok(Callable $closure) {
    lives_ok($closure, '');
}

proto sub eval_dies_ok(|$) is export { * }
multi sub eval_dies_ok(Str $code, $reason) {
    my $ee = eval_exception($code);
    if defined $ee {
        # XXX no regexes yet in nom
        my $bad_death = 0; #"$ee" ~~ / ^ 'Null PMC access ' /;
        if $bad_death {
            diag "wrong way to die: '$ee'";
        }
        proclaim( !$bad_death, $reason );
    }
    else {
        proclaim( 0, $reason );
    }
}
multi sub eval_dies_ok(Str $code) {
    eval_dies_ok($code, '');
}

proto sub eval_lives_ok(|$) is export { * }
multi sub eval_lives_ok(Str $code, $reason) {
    proclaim((not defined eval_exception($code)), $reason);
}
multi sub eval_lives_ok(Str $code) {
    eval_lives_ok($code, '');
}

proto sub is_deeply(|$) is export { * }
multi sub is_deeply(Mu $got, Mu $expected, $reason = '')
{
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
}

sub _is_deeply(Mu $got, Mu $expected) {
    $got eqv $expected;
}


## 'private' subs

sub eval_exception($code) {
    eval ($code);
    $!;
}

sub proclaim($cond, $desc) {
    # exclude the time spent in proclaim from the test time
    $time_after = nqp::p6box_n(pir::time__N);
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
    # exclude the time spent in proclaim from the test time
    $time_before = nqp::p6box_n(pir::time__N);
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
