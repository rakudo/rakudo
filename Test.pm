module Test;
# Copyright (C) 2007 - 2010 The Perl Foundation.

## This is a temporary Test.pm to get us started until we get pugs's Test.pm
## working. It's shamelessly stolen & adapted from MiniPerl6 in the pugs repo.

# globals to keep track of our tests
our $num_of_tests_run    = 0;
our $num_of_tests_failed = 0;
our $todo_upto_test_num  = 0;
our $todo_reason         = '';
our $num_of_tests_planned;
our $no_plan = 1;
our $die_on_fail;

## If done_testing hasn't been run when we hit our END block, we need to know
## so that it can be run. This allows compatibility with old tests that use
## plans and don't call done_testing.
our $done_testing_has_been_run = 0;


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

multi sub plan($number_of_tests) is export {
    if $number_of_tests ~~ ::Whatever {
        $no_plan = 1;
    }
    else {
        $num_of_tests_planned = $number_of_tests;
        $no_plan = 0;

        say '1..' ~ $number_of_tests;
    }
    # Emit two successive timestamps to measure the measurment overhead,
    # and to eliminate cacheing bias, if it exists, from the first test.
    say '# t=' ~ now.to-posix[0] if %*ENV{'PERL6_TEST_TIMES'};
    say '# t=' ~ now.to-posix[0] if %*ENV{'PERL6_TEST_TIMES'};
}

multi sub pass($desc) is export {
    proclaim(1, $desc);
}

multi sub ok(Mu $cond, $desc) is export {
    proclaim(?$cond, $desc);
}

multi sub ok(Mu $cond) is export { ok(?$cond, ''); }


multi sub nok(Mu $cond, $desc) is export {
    proclaim(!$cond, $desc);
}

multi sub nok(Mu $cond) is export { nok($cond, ''); }


multi sub is(Mu $got, Mu $expected, $desc) is export {
    $got.defined; # Hack to deal with Failures
    my $test = $got eq $expected;
    proclaim(?$test, $desc);
    if !$test {
        diag "     got: '$got'";
        diag "expected: '$expected'";
    }
    $test;
}

multi sub is(Mu $got, Mu $expected) is export { is($got, $expected, ''); }


multi sub isnt(Mu $got, Mu $expected, $desc) is export {
    my $test = !($got eq $expected);
    proclaim($test, $desc);
}

multi sub isnt(Mu $got, Mu $expected) is export { isnt($got, $expected, ''); }

multi sub is_approx(Mu $got, Mu $expected, $desc) is export {
    my $test = $got !~~ NaN && ($got - $expected).abs <= 1/100000;
    proclaim(?$test, $desc);
    unless $test {
        diag("got:      $got");
        diag("expected: $expected");
    }
    ?$test;
}

multi sub is_approx(Mu $got, Mu $expected) is export {
    is_approx($got, $expected, '');
}

multi sub todo($reason, $count) is export {
    $todo_upto_test_num = $num_of_tests_run + $count;
    $todo_reason = '# TODO ' ~ $reason;
}

multi sub todo($reason) is export {
    $todo_upto_test_num = $num_of_tests_run + 1;
    $todo_reason = '# TODO ' ~ $reason;
}

multi sub skip()                is export { proclaim(1, "# SKIP"); }
multi sub skip($reason)         is export { proclaim(1, "# SKIP " ~ $reason); }
multi sub skip($reason, $count) is export {
    die "skip() was passed a non-numeric number of tests.  Did you get the arguments backwards?" if $count !~~ Numeric;
    my $i = 1;
    while $i <= $count { proclaim(1, "# SKIP " ~ $reason); $i = $i + 1; }
}

sub skip_rest($reason = '<unknown>') is export {
    skip($reason, $num_of_tests_planned - $num_of_tests_run);
}

sub diag($message) is export {
    say $message.subst(rx/^^/, '# ', :g);
}


multi sub flunk($reason) is export { proclaim(0, "flunk $reason")}


multi sub isa_ok(Mu $var, Mu $type) is export {
    ok($var.isa($type), "The object is-a '$type'")
        or diag('Actual type: ' ~ $var.WHAT);
}
multi sub isa_ok(Mu $var, Mu $type, $msg) is export {
    ok($var.isa($type), $msg)
        or diag('Actual type: ' ~ $var.WHAT);
}

multi sub dies_ok(Callable $closure, $reason) is export {
    my $death = 0;
    my $bad_death = 0;
    try {
        $closure();
        CATCH {
            $death = 1;
            when / ^ 'Null PMC access ' / {
                diag "wrong way to die: '$!'";
                $bad_death = 1;
            }
        }
    }
    proclaim( $death && !$bad_death, $reason );
}
multi sub dies_ok(Callable $closure) is export {
    dies_ok($closure, '');
}

multi sub lives_ok(Callable $closure, $reason) is export {
    try {
        $closure();
    }
    proclaim((not defined $!), $reason);
}
multi sub lives_ok(Callable $closure) is export {
    lives_ok($closure, '');
}

multi sub eval_dies_ok(Str $code, $reason) is export {
    my $ee = eval_exception($code);
    if defined $ee {
        my $bad_death = "$ee" ~~ / ^ 'Null PMC access ' /;
        if $bad_death {
            diag "wrong way to die: '$ee'";
        }
        proclaim( !$bad_death, $reason );
    }
    else {
        proclaim( 0, $reason );
    }
}
multi sub eval_dies_ok(Str $code) is export {
    eval_dies_ok($code, '');
}

multi sub eval_lives_ok(Str $code, $reason) is export {
    proclaim((not defined eval_exception($code)), $reason);
}
multi sub eval_lives_ok(Str $code) is export {
    eval_lives_ok($code, '');
}


multi sub is_deeply(Mu $got, Mu $expected, $reason = '')
                                                is export
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
    say '# t=' ~ now.to-posix[0] if %*ENV{'PERL6_TEST_TIMES'};

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
    our $done_testing_has_been_run;

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
    our $done_testing_has_been_run;
    our $no_plan;

    ## In planned mode, people don't necessarily expect to have to call done
    ## So call it for them if they didn't
    if !$done_testing_has_been_run && !$no_plan {
        done;
    }
}

# vim: ft=perl6
