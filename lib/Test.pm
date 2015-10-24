use nqp;

unit module Test;
# Copyright (C) 2007 - 2014 The Perl Foundation.

# settable from outside
my $perl6_test_times = ? %*ENV<PERL6_TEST_TIMES>;

# global state
my @vars;
my $indents = "";

# variables to keep track of our tests
my $num_of_tests_run;
my $num_of_tests_failed;
my $todo_upto_test_num;
my $todo_reason;
my $num_of_tests_planned;
my $no_plan;
my $die_on_fail;
my num $time_before;
my num $time_after;

# Output should always go to real stdout/stderr, not to any dynamic overrides.
my $output;
my $failure_output;
my $todo_output;

## If done_testing hasn't been run when we hit our END block, we need to know
## so that it can be run. This allows compatibility with old tests that use
## plans and don't call done_testing.
my $done_testing_has_been_run = 0;

# make sure we have initializations
_init_vars();

sub _init_io {
    $output         = $PROCESS::OUT;
    $failure_output = $PROCESS::ERR;
    $todo_output    = $PROCESS::OUT;
}

## test functions

our sub output is rw {
    $output
}

our sub failure_output is rw {
    $failure_output
}

our sub todo_output is rw {
    $todo_output
}

# you can call die_on_fail; to turn it on and die_on_fail(0) to turn it off
sub die_on_fail($fail=1) {
    $die_on_fail = $fail;
}

# "plan 'no_plan';" is now "plan *;"
# It is also the default if nobody calls plan at all
multi sub plan($number_of_tests) is export {
    _init_io() unless $output;
    if $number_of_tests ~~ ::Whatever {
        $no_plan = 1;
    }
    else {
        $num_of_tests_planned = $number_of_tests;
        $no_plan = 0;

        $output.say: $indents ~ '1..' ~ $number_of_tests;
    }
    # Get two successive timestamps to say how long it takes to read the
    # clock, and to let the first test timing work just like the rest.
    # These readings should be made with the expression now.to-posix[0],
    # but its execution time when tried in the following two lines is a
    # lot slower than the non portable nqp::time_n.
    $time_before = nqp::time_n;
    $time_after  = nqp::time_n;
    $output.say: $indents
      ~ '# between two timestamps '
      ~ ceiling(($time_after-$time_before)*1_000_000) ~ ' microseconds'
        if $perl6_test_times;
    # Take one more reading to serve as the begin time of the first test
    $time_before = nqp::time_n;
}

multi sub pass($desc = '') is export {
    $time_after = nqp::time_n;
    proclaim(1, $desc);
    $time_before = nqp::time_n;
}

multi sub ok(Mu $cond, $desc = '') is export {
    $time_after = nqp::time_n;
    my $ok = proclaim(?$cond, $desc);
    $time_before = nqp::time_n;
    $ok;
}

multi sub nok(Mu $cond, $desc = '') is export {
    $time_after = nqp::time_n;
    my $ok = proclaim(!$cond, $desc);
    $time_before = nqp::time_n;
    $ok;
}

multi sub is(Mu $got, Mu:U $expected, $desc = '') is export {
    $time_after = nqp::time_n;
    my $ok;
    if $got.defined { # also hack to deal with Failures
        $ok = proclaim(False, $desc);
        diag "expected: ($expected.^name())";
        diag "     got: '$got'";
    }
    else {
        my $test = $got === $expected;
        $ok = proclaim(?$test, $desc);
        if !$test {
            diag "expected: ($expected.^name())";
            diag "     got: ($got.^name())";
        }
    }
    $time_before = nqp::time_n;
    $ok;
}

multi sub is(Mu $got, Mu:D $expected, $desc = '') is export {
    $time_after = nqp::time_n;
    my $ok;
    if $got.defined { # also hack to deal with Failures
        my $test = $got eq $expected;
        $ok = proclaim(?$test, $desc);
        if !$test {
            if try [eq] ($got, $expected)>>.Str>>.subst(/\s/, '', :g) {
                # only white space differs, so better show it to the user
                diag "expected: {$expected.perl}";
                diag "     got: {$got.perl}";
            }
            else {
                diag "expected: '$expected'";
                diag "     got: '$got'";
            }
        }
    }
    else {
        $ok = proclaim(False, $desc);
        diag "expected: '$expected'";
        diag "     got: ($got.^name())";
    }
    $time_before = nqp::time_n;
    $ok;
}

multi sub isnt(Mu $got, Mu:U $expected, $desc = '') is export {
    $time_after = nqp::time_n;
    my $ok;
    if $got.defined { # also hack to deal with Failures
        $ok = proclaim(True, $desc);
    }
    else {
        my $test = $got !=== $expected;
        $ok = proclaim(?$test, $desc);
        if !$test {
            diag "twice: ($got.^name())";
        }
    }
    $time_before = nqp::time_n;
    $ok;
}

multi sub isnt(Mu $got, Mu:D $expected, $desc = '') is export {
    $time_after = nqp::time_n;
    my $ok;
    if $got.defined { # also hack to deal with Failures
        my $test = $got ne $expected;
        $ok = proclaim(?$test, $desc);
        if !$test {
            diag "twice: '$got'";
        }
    }
    else {
        $ok = proclaim(True, $desc);
    }
    $time_before = nqp::time_n;
    $ok;
}

multi sub cmp-ok(Mu $got, $op, Mu $expected, $desc = '') is export {
    $time_after = nqp::p6box_n(nqp::time_n);
    $got.defined; # Hack to deal with Failures
    my $ok;
    if $op ~~ Callable ?? $op !! try EVAL "&infix:<$op>" -> $matcher {
        $ok = proclaim(?$matcher($got,$expected), $desc);
        if !$ok {
            diag "expected: '{$expected // $expected.^name}'";
            diag " matcher: '$matcher'";
            diag "     got: '$got'";
        }
    }
    else {
        $ok = proclaim(False, $desc);
        diag "Could not use '$op' as a comparator";
    }
    $time_before = nqp::time_n;
    $ok;
}

multi sub is_approx(Mu $got, Mu $expected, $desc = '') is export {
    $time_after = nqp::p6box_n(nqp::time_n);
    my $tol = $expected.abs < 1e-6 ?? 1e-5 !! $expected.abs * 1e-6;
    my $test = ($got - $expected).abs <= $tol;
    my $ok = proclaim(?$test, $desc);
    unless $test {
        diag("expected: $expected");
        diag("got:      $got");
    }
    $time_before = nqp::time_n;
    $ok;
}

multi sub is-approx(Numeric $got, Numeric $expected, $desc = '') is export {
    is-approx($got, $expected, 1e-6, $desc);
}

multi sub is-approx(Numeric $got, Numeric $expected, Numeric $tol, $desc = '') is export {
    $time_after = nqp::p6box_n(nqp::time_n);
    die "Tolerance must be a positive number greater than zero" unless $tol > 0;
    my $abs-diff = ($got - $expected).abs;
    my $abs-max = max($got.abs, $expected.abs);
    my $rel-diff = $abs-max == 0 ?? 0 !! $abs-diff/$abs-max;
    my $test = $rel-diff <= $tol;
    my $ok = proclaim(?$test, $desc);
    unless $test {
        diag("expected: $expected");
        diag("got:      $got");
    }
    $time_before = nqp::p6box_n(nqp::time_n);
    $ok;
}

multi sub is-approx(Numeric $got, Numeric $expected,
                    Numeric :$rel_tol = 1e-6, Numeric :$abs_tol = 0,
		    :$desc = '') is export {
    $time_after = nqp::p6box_n(nqp::time_n);
    die "Relative tolerance must be a positive number greater than zero" unless $rel_tol > 0;
    die "Absolute tolerance must be a positive number greater than zero" unless $abs_tol > 0;
    my $abs-diff = ($got - $expected).abs;
    my $test = (($abs-diff <= ($rel_tol * $expected).abs) &&
	       ($abs-diff <= ($rel_tol * $got).abs) ||
	       ($abs-diff <= $abs_tol));
    my $ok = proclaim(?$test, $desc);
    unless $test {
        diag("expected: $expected");
        diag("got:      $got");
    }
    $time_before = nqp::time_n;
    $ok;
}

multi sub todo($reason, $count = 1) is export {
    $time_after = nqp::time_n;
    $todo_upto_test_num = $num_of_tests_run + $count;
    $todo_reason = '# TODO ' ~ $reason.subst(:g, '#', '\\#');
    $time_before = nqp::time_n;
}

multi sub skip() {
    $time_after = nqp::time_n;
    proclaim(1, "# SKIP");
    $time_before = nqp::time_n;
}
multi sub skip($reason, $count = 1) is export {
    $time_after = nqp::time_n;
    die "skip() was passed a non-numeric number of tests.  Did you get the arguments backwards?" if $count !~~ Numeric;
    my $i = 1;
    while $i <= $count { proclaim(1, "# SKIP " ~ $reason); $i = $i + 1; }
    $time_before = nqp::time_n;
}

sub skip-rest($reason = '<unknown>') is export {
    $time_after = nqp::p6box_n(nqp::time_n);
    die "A plan is required in order to use skip-rest" if $no_plan;
    skip($reason, $num_of_tests_planned - $num_of_tests_run);
    $time_before = nqp::time_n;
}

sub subtest(&subtests, $desc = '') is export {
    _push_vars();
    _init_vars();
    $indents ~= "    ";
    subtests();
    done-testing() if !$done_testing_has_been_run;
    my $status =
      $num_of_tests_failed == 0 && $num_of_tests_planned == $num_of_tests_run;
    _pop_vars;
    $indents .= chop(4);
    proclaim($status,$desc);
}

sub diag(Mu $message) is export {
    _init_io() unless $output;
    my $is_todo = $num_of_tests_run <= $todo_upto_test_num;
    my $out     = $is_todo ?? $todo_output !! $failure_output;

    $time_after = nqp::time_n;
    my $str-message = $message.Str.subst(rx/^^/, '# ', :g);
    $str-message .= subst(rx/^^'#' \s+ $$/, '', :g);
    $out.say: $indents ~ $str-message;
    $time_before = nqp::time_n;
}

multi sub flunk($reason) is export {
    $time_after = nqp::time_n;
    my $ok = proclaim(0, $reason);
    $time_before = nqp::time_n;
    $ok;
}

multi sub isa-ok(Mu $var, Mu $type, $msg = ("The object is-a '" ~ $type.perl ~ "'")) is export {
    $time_after = nqp::p6box_n(nqp::time_n);
    my $ok = proclaim($var.isa($type), $msg)
        or diag('Actual type: ' ~ $var.^name);
    $time_before = nqp::time_n;
    $ok;
}

multi sub does-ok(Mu $var, Mu $type, $msg = ("The object does role '" ~ $type.perl ~ "'")) is export {
    $time_after = nqp::p6box_n(nqp::time_n);
    my $ok = proclaim($var.does($type), $msg)
        or diag([~] 'Type: ',  $var.^name, " doesn't do role ", $type.perl);
    $time_before = nqp::time_n;
    $ok;
}

multi sub can-ok(Mu $var, Str $meth, $msg = ( ($var.defined ?? "An object of type '" !! "The type '" ) ~ $var.WHAT.perl ~ "' can do the method '$meth'") ) is export {
    $time_after = nqp::p6box_n(nqp::time_n);
    my $ok = proclaim($var.^can($meth), $msg);
    $time_before = nqp::time_n;
    $ok;
}

multi sub like(Str $got, Regex $expected, $desc = '') is export {
    $time_after = nqp::time_n;
    $got.defined; # Hack to deal with Failures
    my $test = $got ~~ $expected;
    my $ok = proclaim(?$test, $desc);
    if !$test {
        diag sprintf "     expected: '%s'", $expected.perl;
        diag "     got: '$got'";
    }
    $time_before = nqp::time_n;
    $ok;
}

multi sub unlike(Str $got, Regex $expected, $desc = '') is export {
    $time_after = nqp::time_n;
    $got.defined; # Hack to deal with Failures
    my $test = !($got ~~ $expected);
    my $ok = proclaim(?$test, $desc);
    if !$test {
        diag sprintf "     expected: '%s'", $expected.perl;
        diag "     got: '$got'";
    }
    $time_before = nqp::time_n;
    $ok;
}

multi sub use-ok(Str $code, $msg = ("The module can be use-d ok")) is export {
    $time_after = nqp::time_n;
    try {
	EVAL ( "use $code" );
    }
    my $ok = proclaim((not defined $!), $msg) or diag($!);
    $time_before = nqp::time_n;
    $ok;
}

multi sub dies-ok(Callable $code, $reason = '') is export {
    $time_after = nqp::p6box_n(nqp::time_n);
    my $death = 1;
    try {
        $code();
        $death = 0;
    }
    my $ok = proclaim( $death, $reason );
    $time_before = nqp::time_n;
    $ok;
}

multi sub lives-ok(Callable $code, $reason = '') is export {
    $time_after = nqp::p6box_n(nqp::time_n);
    try {
        $code();
    }
    my $ok = proclaim((not defined $!), $reason) or diag($!);
    $time_before = nqp::time_n;
    $ok;
}

multi sub eval-dies-ok(Str $code, $reason = '') is export {
    $time_after = nqp::p6box_n(nqp::time_n);
    my $ee = eval_exception($code);
    my $ok = proclaim( $ee.defined, $reason );
    $time_before = nqp::time_n;
    $ok;
}

multi sub eval-lives-ok(Str $code, $reason = '') is export {
    $time_after = nqp::p6box_n(nqp::time_n);
    my $ee = eval_exception($code);
    my $ok = proclaim((not defined $ee), $reason)
        or diag("Error: $ee");
    $time_before = nqp::time_n;
    $ok;
}

multi sub is-deeply(Mu $got, Mu $expected, $reason = '') is export {
    $time_after = nqp::p6box_n(nqp::time_n);
    my $test = _is_deeply( $got, $expected );
    my $ok = proclaim($test, $reason);
    if !$test {
        my $got_perl      = try { $got.perl };
        my $expected_perl = try { $expected.perl };
        if $got_perl.defined && $expected_perl.defined {
            diag "expected: $expected_perl";
            diag "     got: $got_perl";
        }
    }
    $time_before = nqp::time_n;
    $ok;
}

sub throws-like($code, $ex_type, $reason?, *%matcher) is export {
    subtest {
        plan 2 + %matcher.keys.elems;
        my $msg;
        if $code ~~ Callable {
            $msg = 'code dies';
            $code()
        } else {
            $msg = "'$code' died";
            EVAL $code, context => CALLER::CALLER::CALLER::CALLER::;
        }
        flunk $msg;
        skip 'Code did not die, can not check exception', 1 + %matcher.elems;
        CATCH {
            default {
                pass $msg;
                my $type_ok = $_ ~~ $ex_type;
                ok $type_ok , "right exception type ({$ex_type.^name})";
                if $type_ok {
                    for %matcher.kv -> $k, $v {
                        my $got is default(Nil) = $_."$k"();
                        my $ok = $got ~~ $v,;
                        ok $ok, ".$k matches $v.gist()";
                        unless $ok {
                            diag "Expected: " ~ ($v ~~ Str ?? $v !! $v.perl);
                            diag "Got:      $got";
                        }
                    }
                } else {
                    diag "Expected: {$ex_type.gist}";
                    diag "Got:      {$_.WHAT.gist}";
                    diag "Exception message: $_.message()";
                    skip 'wrong exception type', %matcher.elems;
                }
            }
        }
    }, $reason // "did we throws-like {$ex_type.^name}?";
}

sub _is_deeply(Mu $got, Mu $expected) {
    $got eqv $expected;
}


## 'private' subs

sub eval_exception($code) {
    try {
        EVAL ($code);
    }
    $!;
}

sub proclaim($cond, $desc) {
    _init_io() unless $output;
    # exclude the time spent in proclaim from the test time
    $num_of_tests_run = $num_of_tests_run + 1;

    my $tap = $indents;
    unless $cond {
        $tap ~= "not ";
        unless  $num_of_tests_run <= $todo_upto_test_num {
            $num_of_tests_failed = $num_of_tests_failed + 1
        }
    }
    if $todo_reason and $num_of_tests_run <= $todo_upto_test_num {
        # TAP parsers do not like '#' in the description, they'd miss the '# TODO'
        $tap ~= "ok $num_of_tests_run - " ~ $desc.subst('#', '', :g) ~ $todo_reason;
    }
    else {
        $tap ~= "ok $num_of_tests_run - $desc";
    }
    $output.say: $tap;
    $output.say: $indents
      ~ "# t="
      ~ ceiling(($time_after-$time_before)*1_000_000)
        if $perl6_test_times;

    unless $cond {
        my $caller;
        my $level = 2; # sub proclaim is not called directly, so 2 is minimum level
        repeat until !$?FILE.ends-with($caller.file) {
            $caller = callframe($level++);
        }
        if $desc ne '' {
            diag "\nFailed test '$desc'\nat {$caller.file} line {$caller.line}";
        } else {
            diag "\nFailed test at {$caller.file} line {$caller.line}";
        }
    }

    if !$cond && $die_on_fail && !$todo_reason {
        die "Test failed.  Stopping test";
    }
    # must clear this between tests
    if $todo_upto_test_num == $num_of_tests_run { $todo_reason = '' }
    $cond;
}

sub done-testing() is export {
    _init_io() unless $output;
    $done_testing_has_been_run = 1;

    if $no_plan {
        $num_of_tests_planned = $num_of_tests_run;
        $output.say: $indents ~ "1..$num_of_tests_planned";
    }

    if ($num_of_tests_planned or $num_of_tests_run) && ($num_of_tests_planned != $num_of_tests_run) {  ##Wrong quantity of tests
        diag("Looks like you planned $num_of_tests_planned test{ $num_of_tests_planned == 1 ?? '' !! 's'}, but ran $num_of_tests_run");
    }
    if ($num_of_tests_failed) {
        diag("Looks like you failed $num_of_tests_failed test{ $num_of_tests_failed == 1 ?? '' !! 's'} of $num_of_tests_run");
    }
}

sub _init_vars {
    $num_of_tests_run     = 0;
    $num_of_tests_failed  = 0;
    $todo_upto_test_num   = 0;
    $todo_reason          = '';
    $num_of_tests_planned = Any;
    $no_plan              = 1;
    $die_on_fail          = Any;
    $time_before          = NaN;
    $time_after           = NaN;
    $done_testing_has_been_run = 0;
}

sub _push_vars {
    @vars.push: item [
      $num_of_tests_run,
      $num_of_tests_failed,
      $todo_upto_test_num,
      $todo_reason,
      $num_of_tests_planned,
      $no_plan,
      $die_on_fail,
      $time_before,
      $time_after,
      $done_testing_has_been_run,
    ];
}

sub _pop_vars {
    (
      $num_of_tests_run,
      $num_of_tests_failed,
      $todo_upto_test_num,
      $todo_reason,
      $num_of_tests_planned,
      $no_plan,
      $die_on_fail,
      $time_before,
      $time_after,
      $done_testing_has_been_run,
    ) = @(@vars.pop);
}

END {
    ## In planned mode, people don't necessarily expect to have to call done
    ## So call it for them if they didn't
    if !$done_testing_has_been_run && !$no_plan {
        done-testing;
    }

    for $output, $failure_output, $todo_output -> $handle {
        next if $handle === ($*ERR|$*OUT);

        $handle.?close;
    }

    if $num_of_tests_failed and $num_of_tests_failed > 0 {
        exit($num_of_tests_failed min 254);
    }
    elsif !$no_plan && ($num_of_tests_planned or $num_of_tests_run) && $num_of_tests_planned != $num_of_tests_run {
        exit(255);
    }
    else {
        exit(0);
    }
}

=begin pod

=head1 NAME

Test - Rakudo Testing Library

=head1 SYNOPSIS

  use Test;

=head1 DESCRIPTION

=head1 FUNCTIONS

=head2 throws-like($code, Mu $expected_type, *%matchers)

If C<$code> is C<Callable>, calls it, otherwise C<EVAL>s it,
and expects it thrown an exception.

If an exception is thrown, it is compared to C<$expected_type>.

Then for each key in C<%matchers>, a method of that name is called
on the resulting exception, and its return value smart-matched against
the value.

Each step is counted as a separate test; if one of the first two fails,
the rest of the tests are skipped.

=end pod

# vim: expandtab shiftwidth=4 ft=perl6
