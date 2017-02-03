use MONKEY-GUTS;          # Allow NQP ops.

unit module Test;
# Copyright (C) 2007 - 2017 The Perl Foundation.

# settable from outside
my int $perl6_test_times = ?%*ENV<PERL6_TEST_TIMES>;
my int $die_on_fail      = ?%*ENV<PERL6_TEST_DIE_ON_FAIL>;

# global state
my @vars;
my $indents = "";

# variables to keep track of our tests
my int $num_of_tests_run;
my int $num_of_tests_failed;
my int $todo_upto_test_num;
my $todo_reason;
my $num_of_tests_planned;
my int $no_plan;
my num $time_before;
my num $time_after;
my int $subtest_level = 0;

# Output should always go to real stdout/stderr, not to any dynamic overrides.
my $output;
my $failure_output;
my $todo_output;

## If done-testing hasn't been run when we hit our END block, we need to know
## so that it can be run. This allows compatibility with old tests that use
## plans and don't call done-testing.
my int $done_testing_has_been_run = 0;

# make sure we have initializations
_init_vars();

sub _init_io {
    $output         = $PROCESS::OUT;
    $failure_output = $PROCESS::ERR;
    $todo_output    = $PROCESS::OUT;
}

sub MONKEY-SEE-NO-EVAL() is export { 1 }

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
        if nqp::iseq_i($perl6_test_times,1);
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
    $ok or ($die_on_fail and die-on-fail) or $ok;
}

multi sub nok(Mu $cond, $desc = '') is export {
    $time_after = nqp::time_n;
    my $ok = proclaim(!$cond, $desc);
    $time_before = nqp::time_n;
    $ok or ($die_on_fail and die-on-fail) or $ok;
}

multi sub is(Mu $got, Mu:U $expected, $desc = '') is export {
    $time_after = nqp::time_n;
    my $ok;
    if $got.defined { # also hack to deal with Failures
        $ok = proclaim(False, $desc);
        _diag "expected: ($expected.^name())\n"
            ~ "     got: '$got'";
    }
    else {
        # infix:<===> can't handle Mu's
        my $test = nqp::eqaddr($expected.WHAT, Mu)
            ?? nqp::eqaddr($got.WHAT, Mu)
                !! nqp::eqaddr($got.WHAT, Mu)
                    ?? False
                    !! $got === $expected;

        $ok = proclaim(?$test, $desc);
        if !$test {
            _diag "expected: ($expected.^name())\n"
                ~ "     got: ($got.^name())";
        }
    }
    $time_before = nqp::time_n;
    $ok or ($die_on_fail and die-on-fail) or $ok;
}

multi sub is(Mu $got, Mu:D $expected, $desc = '') is export {
    $time_after = nqp::time_n;
    my $ok;
    if $got.defined { # also hack to deal with Failures
        # infix:<eq> can't handle Mu's
        my $test = nqp::eqaddr($expected.WHAT, Mu)
            ?? nqp::eqaddr($got.WHAT, Mu)
                !! nqp::eqaddr($got.WHAT, Mu)
                    ?? False
                    !! $got eq $expected;
        $ok = proclaim(?$test, $desc);
        if !$test {
            if try    $got     .Str.subst(/\s/, '', :g)
                   eq $expected.Str.subst(/\s/, '', :g)
            {
                # only white space differs, so better show it to the user
                _diag "expected: {$expected.perl}\n"
                    ~ "     got: {$got.perl}";
            }
            else {
                _diag "expected: '$expected'\n"
                    ~ "     got: '$got'";
            }
        }
    }
    else {
        $ok = proclaim(False, $desc);
        _diag "expected: '$expected'\n"
            ~ "     got: ($got.^name())";
    }
    $time_before = nqp::time_n;
    $ok or ($die_on_fail and die-on-fail) or $ok;
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
            _diag "expected: anything except '$expected.perl()'\n"
                ~ "     got: '$got.perl()'";
        }
    }
    $time_before = nqp::time_n;
    $ok or ($die_on_fail and die-on-fail) or $ok;
}

multi sub isnt(Mu $got, Mu:D $expected, $desc = '') is export {
    $time_after = nqp::time_n;
    my $ok;
    if $got.defined { # also hack to deal with Failures
        my $test = $got ne $expected;
        $ok = proclaim(?$test, $desc);
        if !$test {
            _diag "expected: anything except '$expected.perl()'\n"
                ~ "     got: '$got.perl()'";
        }
    }
    else {
        $ok = proclaim(True, $desc);
    }
    $time_before = nqp::time_n;
    $ok or ($die_on_fail and die-on-fail) or $ok;
}

multi sub cmp-ok(Mu $got, $op, Mu $expected, $desc = '') is export {
    $time_after = nqp::time_n;
    $got.defined; # Hack to deal with Failures
    my $ok;

    # the three labeled &CALLERS below are as follows:
    #  #1 handles ops that don't have '<' or '>'
    #  #2 handles ops that don't have '«' or '»'
    #  #3 handles all the rest by escaping '<' and '>' with backslashes.
    #     Note: #3 doesn't eliminate #1, as #3 fails with '<' operator
    my $matcher = $op ~~ Callable ?? $op
        !! &CALLERS::("infix:<$op>") #1
            // &CALLERS::("infix:«$op»") #2
            // &CALLERS::("infix:<$op.subst(/<?before <[<>]>>/, "\\", :g)>"); #3

    if $matcher {
        $ok = proclaim(?$matcher($got,$expected), $desc);
        if !$ok {
            _diag "expected: '{$expected // $expected.^name}'\n"
                ~ " matcher: '{$matcher.?name || $matcher.^name}'\n"
                ~ "     got: '$got'";
        }
    }
    else {
        $ok = proclaim(False, $desc);
        _diag "Could not use '$op.perl()' as a comparator.";
    }
    $time_before = nqp::time_n;
    $ok or ($die_on_fail and die-on-fail) or $ok;
}

sub bail-out ($desc?) is export {
    $output.put: join ' ', 'Bail out!', ($desc if $desc);
    $done_testing_has_been_run = 1;
    exit 255;
}

multi sub is_approx(Mu $got, Mu $expected, $desc = '') is export {
    DEPRECATED('is-approx'); # Remove for 6.d release

    $time_after = nqp::time_n;
    my $tol = $expected.abs < 1e-6 ?? 1e-5 !! $expected.abs * 1e-6;
    my $test = ($got - $expected).abs <= $tol;
    my $ok = proclaim(?$test, $desc);
    unless $test {
        _diag "expected: $expected\n"
            ~ "got:      $got";
    }
    $time_before = nqp::time_n;
    $ok or ($die_on_fail and die-on-fail) or $ok;
}

# We're picking and choosing which tolerance to use here, to make it easier
# to test numbers close to zero, yet maintain relative tolerance elsewhere.
# For example, relative tolerance works equally well with regular and huge,
# but once we go down to zero, things break down: is-approx sin(τ), 0; would
# fail, because the computed relative tolerance is 1. For such cases, absolute
# tolerance is better suited, so we DWIM in the no-tol version of the sub.
multi sub is-approx(Numeric $got, Numeric $expected, $desc = '') is export {
    $expected.abs < 1e-6
        ?? is-approx-calculate($got, $expected, 1e-5, Nil, $desc) # abs-tol
        !! is-approx-calculate($got, $expected, Nil, 1e-6, $desc) # rel-tol
}

multi sub is-approx(
    Numeric $got, Numeric $expected, Numeric $abs-tol, $desc = ''
) is export {
    is-approx-calculate($got, $expected, $abs-tol, Nil, $desc);
}

multi sub is-approx(
    Numeric $got, Numeric $expected, $desc = '', Numeric :$rel-tol is required
) is export {
    is-approx-calculate($got, $expected, Nil, $rel-tol, $desc);
}

multi sub is-approx(
    Numeric $got, Numeric $expected, $desc = '', Numeric :$abs-tol is required
) is export {
    is-approx-calculate($got, $expected, $abs-tol, Nil, $desc);
}

multi sub is-approx(
    Numeric $got, Numeric $expected, $desc = '',
    Numeric :$rel-tol is required, Numeric :$abs-tol is required
) is export {
    is-approx-calculate($got, $expected, $abs-tol, $rel-tol, $desc);
}

sub is-approx-calculate (
    $got,
    $expected,
    $abs-tol where { !.defined or $_ >= 0 },
    $rel-tol where { !.defined or $_ >= 0 },
    $desc,
) {
    $time_after = nqp::time_n;

    my Bool    ($abs-tol-ok, $rel-tol-ok) = True, True;
    my Numeric ($abs-tol-got, $rel-tol-got);
    if $abs-tol.defined {
        $abs-tol-got = abs($got - $expected);
        $abs-tol-ok = $abs-tol-got <= $abs-tol;
    }
    if $rel-tol.defined {
        if max($got.abs, $expected.abs) -> $max {
            $rel-tol-got = abs($got - $expected) / $max;
            $rel-tol-ok = $rel-tol-got <= $rel-tol;
        }
        else {
            # if $max is zero, then both $got and $expected are zero
            # and so our relative difference is also zero
            $rel-tol-got = 0;
        }
    }

    my $ok = proclaim($abs-tol-ok && $rel-tol-ok, $desc);
    unless $ok {
            _diag "    expected approximately: $expected\n"
                ~ "                       got: $got";

        unless $abs-tol-ok {
            _diag "maximum absolute tolerance: $abs-tol\n"
                ~ "actual absolute difference: $abs-tol-got";
        }
        unless $rel-tol-ok {
            _diag "maximum relative tolerance: $rel-tol\n"
                ~ "actual relative difference: $rel-tol-got";
        }
    }

    $time_before = nqp::time_n;
    $ok or ($die_on_fail and die-on-fail) or $ok;
}

multi sub todo($reason, $count = 1) is export {
    $time_after = nqp::time_n;
    $todo_upto_test_num = $num_of_tests_run + $count;
    # Adding a space to not backslash the # TODO
    $todo_reason = " # TODO $reason";
    $time_before = nqp::time_n;
}

multi sub skip() {
    $time_after = nqp::time_n;
    proclaim(1, "# SKIP");
    $time_before = nqp::time_n;
}
multi sub skip($reason, $count = 1) is export {
    $time_after = nqp::time_n;
    die "skip() was passed a non-integer number of tests.  Did you get the arguments backwards or use a non-integer number?" if $count !~~ Int;
    my $i = 1;
    while $i <= $count { proclaim(1, "# SKIP " ~ $reason); $i = $i + 1; }
    $time_before = nqp::time_n;
}

sub skip-rest($reason = '<unknown>') is export {
    $time_after = nqp::time_n;
    die "A plan is required in order to use skip-rest"
      if nqp::iseq_i($no_plan,1);
    skip($reason, $num_of_tests_planned - $num_of_tests_run);
    $time_before = nqp::time_n;
}

multi sub subtest(Pair $what)            is export { subtest($what.value,$what.key) }
multi sub subtest($desc, &subtests)      is export { subtest(&subtests,$desc)       }
multi sub subtest(&subtests, $desc = '') is export {
    _push_vars();
    _init_vars();
    $indents ~= "    ";
    ## TODO: remove workaround for rakudo-j RT #128123 when postfix:<++> does not die here
    $subtest_level += 1;
    subtests();
    ## TODO: remove workaround for rakudo-j RT #128123 when postfix:<--> does not die here
    $subtest_level -= 1;
    done-testing() if nqp::iseq_i($done_testing_has_been_run,0);
    my $status =
      $num_of_tests_failed == 0 && $num_of_tests_planned == $num_of_tests_run;
    _pop_vars;
    $indents .= chop(4);
    proclaim($status,$desc) or ($die_on_fail and die-on-fail);
}

sub diag(Mu $message) is export {
    # Always send user-triggered diagnostics to STDERR. This prevents
    # cases of confusion of where diag() has to send its ouput when
    # we are in the middle of TODO tests
    _diag $message, :force-stderr;
}

sub _diag(Mu $message, :$force-stderr) {
    _init_io() unless $output;
    my $is_todo = !$force-stderr && $num_of_tests_run <= $todo_upto_test_num;
    my $out     = $is_todo ?? $todo_output !! $failure_output;

    $time_after = nqp::time_n;
    my $str-message = nqp::join(
        "\n$indents# ", nqp::split("\n", "$indents# $message")
    );
    $str-message .= subst(rx/^^ "$indents#" \s+ $$/, '', :g);
    $out.say: $str-message;
    $time_before = nqp::time_n;
}

# In earlier Perls, this is spelled "sub fail"
multi sub flunk($reason) is export {
    $time_after = nqp::time_n;
    my $ok = proclaim(0, $reason);
    $time_before = nqp::time_n;
    $ok or ($die_on_fail and die-on-fail) or $ok;
}

multi sub isa-ok(Mu $var, Mu $type, $msg = ("The object is-a '" ~ $type.perl ~ "'")) is export {
    $time_after = nqp::time_n;
    my $ok = proclaim($var.isa($type), $msg)
        or _diag('Actual type: ' ~ $var.^name);
    $time_before = nqp::time_n;
    $ok or ($die_on_fail and die-on-fail) or $ok;
}

multi sub does-ok(Mu $var, Mu $type, $msg = ("The object does role '" ~ $type.perl ~ "'")) is export {
    $time_after = nqp::time_n;
    my $ok = proclaim($var.does($type), $msg)
        or _diag([~] 'Type: ',  $var.^name, " doesn't do role ", $type.perl);
    $time_before = nqp::time_n;
    $ok or ($die_on_fail and die-on-fail) or $ok;
}

multi sub can-ok(Mu $var, Str $meth, $msg = ( ($var.defined ?? "An object of type '" !! "The type '" ) ~ $var.WHAT.perl ~ "' can do the method '$meth'") ) is export {
    $time_after = nqp::time_n;
    my $ok = proclaim($var.^can($meth), $msg);
    $time_before = nqp::time_n;
    $ok or ($die_on_fail and die-on-fail) or $ok;
}

multi sub like(Str $got, Regex $expected, $desc = '') is export {
    $time_after = nqp::time_n;
    $got.defined; # Hack to deal with Failures
    my $test = $got ~~ $expected;
    my $ok = proclaim(?$test, $desc);
    if !$test {
        _diag "     expected: '$expected.perl()'\n"
            ~ "     got: '$got'";
    }
    $time_before = nqp::time_n;
    $ok or ($die_on_fail and die-on-fail) or $ok;
}

multi sub unlike(Str $got, Regex $expected, $desc = '') is export {
    $time_after = nqp::time_n;
    $got.defined; # Hack to deal with Failures
    my $test = !($got ~~ $expected);
    my $ok = proclaim(?$test, $desc);
    if !$test {
        _diag "     expected: '$expected.perl()'\n"
            ~ "     got: '$got'";
    }
    $time_before = nqp::time_n;
    $ok or ($die_on_fail and die-on-fail) or $ok;
}

multi sub use-ok(Str $code, $msg = ("The module can be use-d ok")) is export {
    $time_after = nqp::time_n;
    try {
        EVAL ( "use $code" );
    }
    my $ok = proclaim((not defined $!), $msg) or _diag($!);
    $time_before = nqp::time_n;
    $ok or ($die_on_fail and die-on-fail) or $ok;
}

multi sub dies-ok(Callable $code, $reason = '') is export {
    $time_after = nqp::time_n;
    my $death = 1;
    try {
        $code();
        $death = 0;
    }
    my $ok = proclaim( $death, $reason );
    $time_before = nqp::time_n;
    $ok or ($die_on_fail and die-on-fail) or $ok;
}

multi sub lives-ok(Callable $code, $reason = '') is export {
    $time_after = nqp::time_n;
    try {
        $code();
    }
    my $ok = proclaim((not defined $!), $reason) or _diag($!);
    $time_before = nqp::time_n;
    $ok or ($die_on_fail and die-on-fail) or $ok;
}

multi sub eval-dies-ok(Str $code, $reason = '') is export {
    $time_after = nqp::time_n;
    my $ee = eval_exception($code);
    my $ok = proclaim( $ee.defined, $reason );
    $time_before = nqp::time_n;
    $ok or ($die_on_fail and die-on-fail) or $ok;
}

multi sub eval-lives-ok(Str $code, $reason = '') is export {
    $time_after = nqp::time_n;
    my $ee = eval_exception($code);
    my $ok = proclaim((not defined $ee), $reason)
        or _diag("Error: $ee");
    $time_before = nqp::time_n;
    $ok or ($die_on_fail and die-on-fail) or $ok;
}

multi sub is-deeply(Seq $got, Seq $expected, $reason = '') is export {
    is-deeply $got.cache, $expected.cache, $reason;
}
multi sub is-deeply(Seq $got, Mu $expected, $reason = '') is export {
    is-deeply $got.cache, $expected, $reason;
}
multi sub is-deeply(Mu $got, Seq $expected, $reason = '') is export {
    is-deeply $got, $expected.cache, $reason;
}
multi sub is-deeply(Mu $got, Mu $expected, $reason = '') is export {
    $time_after = nqp::time_n;
    my $test = _is_deeply( $got, $expected );
    my $ok = proclaim($test, $reason);
    if !$test {
        my $got_perl      = try { $got.perl };
        my $expected_perl = try { $expected.perl };
        if $got_perl.defined && $expected_perl.defined {
            _diag "expected: $expected_perl\n"
                ~ "     got: $got_perl";
        }
    }
    $time_before = nqp::time_n;
    $ok or ($die_on_fail and die-on-fail) or $ok;
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
                            _diag "Expected: " ~ ($v ~~ Str ?? $v !! $v.perl)
                              ~ "\nGot:      $got";
                        }
                    }
                } else {
                    _diag "Expected: {$ex_type.^name}\n"
                        ~ "Got:      {$_.^name}\n"
                        ~ "Exception message: $_.message()";
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

sub die-on-fail {
    if !$todo_reason && !$subtest_level && nqp::iseq_i($die_on_fail,1) {
        _diag 'Test failed. Stopping test suite, because'
                ~ ' PERL6_TEST_DIE_ON_FAIL environmental variable is set'
                ~ ' to a true value.';
        exit 255;
    }

    $todo_reason = '' if $todo_upto_test_num == $num_of_tests_run;

    False;
}

sub eval_exception($code) {
    try {
        EVAL ($code);
    }
    $!;
}

sub proclaim($cond, $desc is copy ) {
    _init_io() unless $output;
    # exclude the time spent in proclaim from the test time
    $num_of_tests_run = $num_of_tests_run + 1;

    my $tap = $indents;
    unless $cond {
        $tap ~= "not ";
        $num_of_tests_failed = $num_of_tests_failed + 1
          unless  $num_of_tests_run <= $todo_upto_test_num;
    }

    # TAP parsers do not like '#' in the description, they'd miss the '# TODO'
    # So, adding a ' \' before it.
    $desc = $desc
    ??  nqp::join(' \\#',
            nqp::split('#',
                $desc.Str
            )
        )
    !! '';

    $tap ~= $todo_reason && $num_of_tests_run <= $todo_upto_test_num
        ?? "ok $num_of_tests_run - $desc$todo_reason"
        !! "ok $num_of_tests_run - $desc";

    $output.say: $tap;
    $output.say: $indents
      ~ "# t="
      ~ ceiling(($time_after-$time_before)*1_000_000)
        if nqp::iseq_i($perl6_test_times,1);

    unless $cond {
        my $caller;
        # sub proclaim is not called directly, so 2 is minimum level
        my int $level = 2;

        repeat until !$?FILE.ends-with($caller.file) {
            $caller = callframe($level++);
        }

        _diag $desc
          ?? "\nFailed test '$desc'\nat {$caller.file} line {$caller.line}"
          !! "\nFailed test at {$caller.file} line {$caller.line}";
    }

    # must clear this between tests
    $todo_reason = ''
        if $todo_upto_test_num == $num_of_tests_run
            and nqp::iseq_i($die_on_fail,0);

    $cond
}

sub done-testing() is export {
    _init_io() unless $output;
    $done_testing_has_been_run = 1;

    if nqp::iseq_i($no_plan,1) {
        $num_of_tests_planned = $num_of_tests_run;
        $output.say: $indents ~ "1..$num_of_tests_planned";
    }

    # Wrong quantity of tests
    _diag("Looks like you planned $num_of_tests_planned test{
        $num_of_tests_planned == 1 ?? '' !! 's'
    }, but ran $num_of_tests_run")
      if ($num_of_tests_planned or $num_of_tests_run)
      && ($num_of_tests_planned != $num_of_tests_run);

    _diag("Looks like you failed $num_of_tests_failed test{
        $num_of_tests_failed == 1 ?? '' !! 's'
    } of $num_of_tests_run") if $num_of_tests_failed;
}

sub _init_vars {
    $num_of_tests_run     = 0;
    $num_of_tests_failed  = 0;
    $todo_upto_test_num   = 0;
    $todo_reason          = '';
    $num_of_tests_planned = Any;
    $no_plan              = 1;
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
      $time_before,
      $time_after,
      $done_testing_has_been_run,
    ) = @(@vars.pop);
}

END {
    ## In planned mode, people don't necessarily expect to have to call done-testing
    ## So call it for them if they didn't
    done-testing
      if nqp::iseq_i($done_testing_has_been_run,0)
      && nqp::iseq_i($no_plan,0);

    .?close unless $_ === $*OUT || $_ === $*ERR
      for $output, $failure_output, $todo_output;

    exit($num_of_tests_failed min 254) if $num_of_tests_failed > 0;

    exit(255)
      if nqp::iseq_i($no_plan,0)
      && ($num_of_tests_planned or $num_of_tests_run)
      &&  $num_of_tests_planned != $num_of_tests_run;
}

=begin pod

=head1 NAME

Test - Rakudo Testing Library

=head1 SYNOPSIS

  use Test;

=head1 DESCRIPTION

Please check the section Language/testing of the doc repository.
If you have 'p6doc' installed, you can do 'p6doc Language/testing'.

You can also check the documentation about testing in Perl 6 online on:

  https://doc.perl6.org/language/testing

=end pod

# vim: expandtab shiftwidth=4 ft=perl6
