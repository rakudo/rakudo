use v6.c;
use MONKEY-GUTS;          # Allow NQP ops.

unit module Test;
# Copyright (C) 2007 - 2021 The Perl Foundation.

# settable from outside
my int $raku_test_times =
  ?(%*ENV<RAKU_TEST_TIMES> // %*ENV<PERL6_TEST_TIMES>);
my int $die_on_fail =
  ?(%*ENV<RAKU_TEST_DIE_ON_FAIL> // %*ENV<PERL6_TEST_DIE_ON_FAIL>);

# global state
my @vars;
my $indents = "";

# variables to keep track of our tests
my $subtest_callable_type;
my int $num_of_tests_run;
my int $num_of_tests_failed;
my int $todo_upto_test_num;
my $todo_reason;
my $num_of_tests_planned;
my int $no_plan;
my int $time_before;
my int $time_after;
my int $subtest_level;
my $subtest_todo_reason;
my int $pseudo_fails; # number of untodoed failed tests inside a todoed subtest

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
    nqp::setbuffersizefh(nqp::getstdout(), 0);
    nqp::setbuffersizefh(nqp::getstderr(), 0);
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

multi sub trait_mod:<is>(Routine:D $r, :$test-assertion!) is export {
    $r.^mixin( role is-test-assertion {
        method is-test-assertion(--> True) { }
    }) if $test-assertion;
}

proto sub plan ($?, Cool :$skip-all) {*}

my class X::SubtestsSkipped is Exception {}

multi sub plan (Cool:D :skip-all($reason)!) {
    _init_io() unless $output;
    $output.say: $indents ~ "1..0 # Skipped: $reason";

    exit unless $subtest_level; # just exit if we're not in a subtest

    # but if we are, adjust the vars, so the output matches up with zero
    # planned tests, all passsing. Also, check the subtest's Callable is
    # something we can actually return from.

    $subtest_callable_type === Sub|Method
        or die "Must give `subtest` a (Sub) or a (Method) to be able to use "
            ~ "`skip-all` plan inside, but you gave a "
            ~ $subtest_callable_type.gist;

    $done_testing_has_been_run = 1;
    $num_of_tests_failed = $num_of_tests_planned = $num_of_tests_run = 0;
    X::SubtestsSkipped.new.throw
}

# "plan 'no_plan';" is now "plan *;"
# It is also the default if nobody calls plan at all
multi sub plan($number_of_tests) is export {
    _init_io() unless $output;

    my str $str-message;

    if $number_of_tests ~~ ::Whatever {
        $no_plan = 1;
    }
    else {
        $num_of_tests_planned = $number_of_tests;
        $no_plan = 0;

        $str-message ~= $indents ~ '1..' ~ $number_of_tests;
    }
    # Get two successive timestamps to say how long it takes to read the
    # clock, and to let the first test timing work just like the rest.
    # These readings should be made with the expression now.to-posix[0],
    # but its execution time when tried in the following two lines is a
    # lot slower than the non portable nqp::time_n.
    $time_before = nqp::time;
    $time_after  = nqp::time;
    $str-message ~= "\n$indents# between two timestamps " ~ ceiling(($time_after-$time_before)*1_000_000) ~ ' microseconds'
        if nqp::iseq_i($raku_test_times,1);

    $output.say: $str-message;

    # Take one more reading to serve as the begin time of the first test
    $time_before = nqp::time;
}

multi sub pass($desc = '') is export {
    $time_after = nqp::time;
    my $ok = proclaim(1, $desc);
    $time_before = nqp::time;
    $ok;
}

multi sub ok(Mu $cond, $desc = '') is export {
    $time_after = nqp::time;
    my $ok = proclaim($cond, $desc);
    $time_before = nqp::time;
    $ok or ($die_on_fail and die-on-fail) or $ok;
}

multi sub nok(Mu $cond, $desc = '') is export {
    $time_after = nqp::time;
    my $ok = proclaim(!$cond, $desc);
    $time_before = nqp::time;
    $ok or ($die_on_fail and die-on-fail) or $ok;
}

multi sub is(Mu $got, Mu:U $expected, $desc = '') is export {
    $time_after = nqp::time;
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

        $ok = proclaim($test, $desc);
        if !$test {
            _diag "expected: ($expected.^name())\n"
                ~ "     got: ($got.^name())";
        }
    }
    $time_before = nqp::time;
    $ok or ($die_on_fail and die-on-fail) or $ok;
}

multi sub is(Mu $got, Mu:D $expected, $desc = '') is export {
    $time_after = nqp::time;
    my $ok;
    if $got.defined { # also hack to deal with Failures
        # infix:<eq> can't handle Mu's
        my $test = nqp::eqaddr($expected.WHAT, Mu)
            ?? nqp::eqaddr($got.WHAT, Mu)
                !! nqp::eqaddr($got.WHAT, Mu)
                    ?? False
                    !! $got eq $expected;
        $ok = proclaim($test, $desc);
        if !$test {
            if try    $got     .Str.subst(/\s/, '', :g)
                   eq $expected.Str.subst(/\s/, '', :g)
            {
                # only white space differs, so better show it to the user
                _diag "expected: $expected.raku()\n"
                    ~ "     got: $got.raku()";
            }
            else {
                 try { # if the type support Stringification
                      # note: we can't use ^can('Str') as Buf error in its Str method itself
                    _diag "expected: '$expected'\n"
                    ~ "     got: '$got'";
                    True;
                } or {
                    _diag "expected: $expected.raku()\n"
                    ~ "     got: $got.raku()";
                }
            }
        }
    }
    else {
        $ok = proclaim(False, $desc);
        _diag "expected: '$expected'\n"
            ~ "     got: ($got.^name())";
    }
    $time_before = nqp::time;
    $ok or ($die_on_fail and die-on-fail) or $ok;
}

multi sub isnt(Mu $got, Mu:U $expected, $desc = '') is export {
    $time_after = nqp::time;
    my $ok;
    if $got.defined { # also hack to deal with Failures
        $ok = proclaim(True, $desc);
    }
    else {
        my $test = $got !=== $expected;
        $ok = proclaim($test, $desc);
        if !$test {
            _diag "expected: anything except '$expected.raku()'\n"
                ~ "     got: '$got.raku()'";
        }
    }
    $time_before = nqp::time;
    $ok or ($die_on_fail and die-on-fail) or $ok;
}

multi sub isnt(Mu $got, Mu:D $expected, $desc = '') is export {
    $time_after = nqp::time;
    my $ok;
    if $got.defined { # also hack to deal with Failures
        my $test = $got ne $expected;
        $ok = proclaim($test, $desc);
        if !$test {
            _diag "expected: anything except '$expected.raku()'\n"
                ~ "     got: '$got.raku()'";
        }
    }
    else {
        $ok = proclaim(True, $desc);
    }
    $time_before = nqp::time;
    $ok or ($die_on_fail and die-on-fail) or $ok;
}

multi sub cmp-ok(Mu $got is raw, $op, Mu $expected is raw, $desc = '') is export {
    $time_after = nqp::time;
    $got.defined if nqp::istype($got, Failure); # Hack to deal with Failures
    my $ok;

    # the three labeled &CALLERS below are as follows:
    #  #1 handles ops that don't have '<' or '>'
    #  #2 handles ops that don't have '«' or '»'
    #  #3 handles all the rest by escaping '<' and '>' with backslashes.
    #     Note: #3 doesn't eliminate #1, as #3 fails with '<' operator
    my $matcher = nqp::istype($op, Callable) ?? $op
        !! &CALLERS::("infix:<$op>") #1
            // &CALLERS::("infix:«$op»") #2
            // &CALLERS::("infix:<$op.subst(/<?before <[<>]>>/, "\\", :g)>"); #3

    if $matcher {
        $ok = proclaim($matcher($got,$expected), $desc);
        if !$ok {
            my $expected-desc = stringify $expected;
            my      $got-desc = stringify $got;
            _diag "expected: $expected-desc\n"
                ~ " matcher: '" ~ ($matcher.?name || $matcher.^name) ~ "'\n"
                ~ "     got: $got-desc";
        }
    }
    else {
        $ok = proclaim(False, $desc);
        _diag "Could not use '$op.raku()' as a comparator." ~ (
            ' If you are trying to use a meta operator, pass it as a '
            ~ "Callable instead of a string: \&[$op]"
            unless nqp::istype($op, Callable)
        );
    }
    $time_before = nqp::time;
    $ok or ($die_on_fail and die-on-fail) or $ok;
}

sub bail-out ($desc?) is export {
    _init_io() unless $output;
    $output.put: join ' ', 'Bail out!', ($desc if $desc);
    $done_testing_has_been_run = 1;
    exit 255;
}

multi sub is_approx(Mu $got, Mu $expected, $desc = '') is export {
    Rakudo::Deprecations.DEPRECATED('is-approx'); # Remove for 6.d release

    $time_after = nqp::time;
    my $tol = $expected.abs < 1e-6 ?? 1e-5 !! $expected.abs * 1e-6;
    my $test = ($got - $expected).abs <= $tol;
    my $ok = proclaim($test, $desc);
    unless $test {
        _diag "expected: $expected\n"
            ~ "got:      $got";
    }
    $time_before = nqp::time;
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
    $time_after = nqp::time;

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

    $time_before = nqp::time;
    $ok or ($die_on_fail and die-on-fail) or $ok;
}

multi sub todo($reason, $count = 1) is export {
    $time_after = nqp::time;
    $todo_upto_test_num = $num_of_tests_run + $count;
    # Adding a space to not backslash the # TODO
    $todo_reason = " # TODO $reason";
    $time_before = nqp::time;
}

multi sub skip() {
    $time_after = nqp::time;
    proclaim(1, '', "# SKIP");
    $time_before = nqp::time;
}
multi sub skip($reason, $count = 1) is export {
    $time_after = nqp::time;
    die "skip() was passed a non-integer number of tests.  Did you get the arguments backwards or use a non-integer number?" if $count !~~ Int;
    my $i = 1;
    while $i <= $count { proclaim(1, $reason, "# SKIP "); $i = $i + 1; }
    $time_before = nqp::time;
}

sub skip-rest($reason = '<unknown>') is export {
    $time_after = nqp::time;
    die "A plan is required in order to use skip-rest"
      if nqp::iseq_i($no_plan,1);
    skip($reason, $num_of_tests_planned - $num_of_tests_run);
    $time_before = nqp::time;
}

multi sub subtest(Pair $what)            is export { subtest($what.value,$what.key) }
multi sub subtest($desc, &subtests)      is export { subtest(&subtests,$desc)       }
multi sub subtest(&subtests, $desc = '') is export {
    _diag "Subtest" ~ ($desc ?? ": " ~ $desc !! ''), :force-informative;
    my $parent_todo = $todo_reason || $subtest_todo_reason;
    _push_vars();
    _init_vars();
    $subtest_todo_reason   = $parent_todo;
    $subtest_callable_type = &subtests.WHAT;
    $indents ~= "    ";
    $subtest_level++;
    {
        subtests();
        CATCH {
            when X::SubtestsSkipped {
                # Subtests all skipped
            }
        }
    }
    $subtest_level--;
    done-testing() if nqp::iseq_i($done_testing_has_been_run,0);
    my $status = $num_of_tests_failed == 0
        && $num_of_tests_planned == $num_of_tests_run
        && $pseudo_fails == 0;
    _pop_vars;
    $indents .= chop(4);
    proclaim($status,$desc) or ($die_on_fail and die-on-fail);
}

sub diag($message) is export {
    # Always send user-triggered diagnostics to STDERR. This prevents
    # cases of confusion of where diag() has to send its ouput when
    # we are in the middle of TODO tests
    _diag $message, :force-stderr;
}

sub _diag($message, :$force-stderr, :$force-informative) {
    _init_io() unless $output;
    my $is_todo = !$force-stderr && !$force-informative
        && ($subtest_todo_reason || $num_of_tests_run <= $todo_upto_test_num);
    my $out     = $is_todo || $force-informative ?? $todo_output !! $failure_output;

    $time_after = nqp::time;
    my $str-message = nqp::join(
        "\n$indents# ", nqp::split("\n", "$indents# $message")
    );
    $out.say: $str-message;
    $time_before = nqp::time;
}

# In earlier Perls, this is spelled "sub fail"
multi sub flunk($reason = '') is export {
    $time_after = nqp::time;
    my $ok = proclaim(0, $reason);
    $time_before = nqp::time;
    $ok or ($die_on_fail and die-on-fail) or $ok;
}

multi sub isa-ok(
    Mu $var, Mu $type, $desc = "The object is-a '$type.raku()'"
) is export {
    $time_after = nqp::time;
    my $ok = ($type ~~ Str:D
                ?? proclaim($var.isa($type), $desc)
                !! proclaim(nqp::istype($var, $type.WHAT), $desc))
        or _diag('Actual type: ' ~ $var.^name);
    $time_before = nqp::time;
    $ok or ($die_on_fail and die-on-fail) or $ok;
}

multi sub does-ok(
    Mu $var, Mu $type, $desc = "The object does role '$type.raku()'"
) is export {
    $time_after = nqp::time;
    my $ok = proclaim($var.does($type), $desc)
        or _diag([~] 'Type: ',  $var.^name, " doesn't do role ", $type.raku);
    $time_before = nqp::time;
    $ok or ($die_on_fail and die-on-fail) or $ok;
}

multi sub can-ok(
    Mu $var, Str $meth,
    $desc = ($var.defined ?? "An object of type '" !! "The type '")
        ~ "$var.WHAT.raku()' can do the method '$meth'"
) is export {
    $time_after = nqp::time;
    my $ok = proclaim($var.^can($meth), $desc);
    $time_before = nqp::time;
    $ok or ($die_on_fail and die-on-fail) or $ok;
}

multi sub like(
    Str() $got, Regex:D $expected,
    $desc = "text matches $expected.raku()"
) is export {
    $time_after = nqp::time;
    my $ok := proclaim $got ~~ $expected, $desc
        or _diag "expected a match with: $expected.raku()\n"
               ~ "                  got: $got.raku()";
    $time_before = nqp::time;
    $ok or ($die_on_fail and die-on-fail) or $ok;
}

multi sub unlike(
    Str() $got, Regex:D $expected,
    $desc = "text does not match $expected.raku()"
) is export {
    $time_after = nqp::time;
    my $ok := proclaim !($got ~~ $expected), $desc
        or _diag "expected no match with: $expected.raku()\n"
               ~ "                   got: $got.raku()";
    $time_before = nqp::time;
    $ok or ($die_on_fail and die-on-fail) or $ok;
}

multi sub use-ok(Str $code, $desc = "$code module can be use-d ok") is export {
    $time_after = nqp::time;
    try {
        EVAL ( "use $code" );
    }
    my $ok = proclaim((not defined $!), $desc) or _diag($!);
    $time_before = nqp::time;
    $ok or ($die_on_fail and die-on-fail) or $ok;
}

multi sub dies-ok(Callable $code, $reason = '') is export {
    $time_after = nqp::time;
    my $death = 1;
    try {
        $code();
        $death = 0;
    }
    my $ok = proclaim( $death, $reason );
    $time_before = nqp::time;
    $ok or ($die_on_fail and die-on-fail) or $ok;
}

multi sub lives-ok(Callable $code, $reason = '') is export {
    $time_after = nqp::time;
    try {
        $code();
    }
    my $ok = proclaim((not defined $!), $reason) or _diag($!);
    $time_before = nqp::time;
    $ok or ($die_on_fail and die-on-fail) or $ok;
}

multi sub eval-dies-ok(Str $code, $reason = '') is export {
    $time_after = nqp::time;
    my $ee = eval_exception($code);
    my $ok = proclaim( $ee.defined, $reason );
    $time_before = nqp::time;
    $ok or ($die_on_fail and die-on-fail) or $ok;
}

multi sub eval-lives-ok(Str $code, $reason = '') is export {
    $time_after = nqp::time;
    my $ee = eval_exception($code);
    my $ok = proclaim((not defined $ee), $reason)
        or _diag("Error: $ee");
    $time_before = nqp::time;
    $ok or ($die_on_fail and die-on-fail) or $ok;
}

######################################################################
# The fact that is-deeply converts Seq args to Lists is actually a bug
# that ended up being too-much-pain-for-little-gain to fix. Using Seqs
# breaks ~65 tests in 6.c-errata and likely breaks a lot of module
# tests as well. So... for the foreseeable future we decided to leave it
# as is. If a user really wants to ensure Seq comparison, there's always
# `cmp-ok` with `eqv` op.
# https://colabti.org/irclogger/irclogger_log/perl6-dev?date=2017-05-04#l100
######################################################################
multi sub is-deeply(Seq:D $got, Seq:D $expected, $reason = '') is export {
    is-deeply $got.cache, $expected.cache, $reason;
}
multi sub is-deeply(Seq:D $got, Mu $expected, $reason = '') is export {
    is-deeply $got.cache, $expected, $reason;
}
multi sub is-deeply(Mu $got, Seq:D $expected, $reason = '') is export {
    is-deeply $got, $expected.cache, $reason;
}
multi sub is-deeply(Mu $got, Mu $expected, $reason = '') is export {
    $time_after = nqp::time;
    my $test = _is_deeply( $got, $expected );
    my $ok = proclaim($test, $reason);
    if !$test {
        my $got_perl      = try { $got.raku };
        my $expected_perl = try { $expected.raku };
        if $got_perl.defined && $expected_perl.defined {
            _diag "expected: $expected_perl\n"
                ~ "     got: $got_perl";
        }
    }
    $time_before = nqp::time;
    $ok or ($die_on_fail and die-on-fail) or $ok;
}

sub throws-like($code, $ex_type, $reason?, *%matcher) is export {
    for %matcher.kv -> $k, $v {
        if $v.DEFINITE && $v ~~ Bool {
            X::Match::Bool.new(:type(".$k")).throw;
        }
    }
    my $caller-context = $*THROWS-LIKE-CONTEXT // CALLER::; # Don't guess our caller context, know it!
    subtest {
        plan 2 + %matcher.keys.elems;
        my $msg;
        if $code ~~ Callable {
            $msg = 'code dies';
            $code()
        } else {
            $msg = "'$code' died";
            EVAL $code, context => $caller-context;
        }
        flunk $msg;
        skip 'Code did not die, can not check exception', 1 + %matcher.elems;
        CATCH {
            default {
                pass $msg;
                my $type_ok = $_ ~~ $ex_type;
                ok $type_ok , "right exception type ($ex_type.^name())";
                if $type_ok {
                    for %matcher.kv -> $k, $v {
                        my $got is default(Nil) = $_."$k"();
                        my $ok = $got ~~ $v,;
                        ok $ok, ".$k matches $v.gist()";
                        unless $ok {
                            _diag "Expected: " ~ ($v ~~ Str ?? $v !! $v.raku)
                              ~ "\nGot:      $got";
                        }
                    }
                } else {
                    _diag "Expected: $ex_type.^name()\n"
                        ~ "Got:      $_.^name()\n"
                        ~ "Exception message: $_.message()";
                    skip 'wrong exception type', %matcher.elems;
                }
            }
        }
    }, $reason // "did we throws-like $ex_type.^name()?";
}

sub fails-like (
    \test where Callable:D|Str:D, $ex-type, $reason?, *%matcher
) is export {
    for %matcher.kv -> $k, $v {
        if $v.DEFINITE && $v ~~ Bool {
            X::Match::Bool.new(:type(".$k")).throw;
        }
    }
    my $*THROWS-LIKE-CONTEXT = CALLER::;
    subtest sub {
        plan 2;
        CATCH { default {
            with "expected code to fail but it threw {.^name} instead" {
                .&flunk;
                .&skip;
                return False;
            }
        }}
        my $res = test ~~ Callable ?? test.() !! test.EVAL;
        isa-ok $res, Failure, 'code returned a Failure';
        throws-like { $res.sink }, $ex-type,
            'Failure threw when sunk', |%matcher,
    }, $reason // "did we fails-like $ex-type.^name()?"
}

sub _is_deeply(Mu $got, Mu $expected) {
    $got eqv $expected;
}


## 'private' subs

sub die-on-fail {
    if !$todo_reason && !$subtest_level && nqp::iseq_i($die_on_fail,1) {
        _diag 'Test failed. Stopping test suite, because the '
          ~ (%*ENV<RAKU_TEST_DIE_ON_FAIL> ?? 'RAKU' !! 'PERL6')
          ~ "_TEST_DIE_ON_FAIL\n"
          ~ 'environmental variable is set to a true value.';
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

# Stringifies values passed to &cmp-ok.
sub stringify(Mu $obj is raw --> Str:D) {
    (try $obj.raku if nqp::can($obj, 'raku'))
        // ($obj.gist if nqp::can($obj, 'gist'))
        // ($obj.HOW.name($obj) if nqp::can($obj.HOW, 'name'))
        // '?'
}

# Take $cond as Mu so we don't thread with Junctions:
sub proclaim(
  Bool(Mu) $cond,
  $desc is copy,
  $unescaped-prefix = ''
) is hidden-from-backtrace {
    _init_io() unless $output;
    # exclude the time spent in proclaim from the test time
    $num_of_tests_run = $num_of_tests_run + 1;

    my $tap = $indents;
    unless $cond {
        $tap ~= "not ";

        $num_of_tests_failed = $num_of_tests_failed + 1
            unless $num_of_tests_run <= $todo_upto_test_num;

        $pseudo_fails = $pseudo_fails + 1 if $subtest_todo_reason;
    }

    # TAP parsers do not like '#' in the description, they'd miss the '# TODO'
    # So, adding a ' \' before it.
    $desc = $desc
    ??  nqp::join("\n$indents# ", # prefix newlines with `#`
            nqp::split("\n",
                nqp::join(' \\#', # escape `#`
                    nqp::split('#', $desc.Str))))
    !! '';

    $tap ~= $todo_reason && $num_of_tests_run <= $todo_upto_test_num
        ?? "ok $num_of_tests_run - $unescaped-prefix$desc$todo_reason"
        !! (! $cond && $subtest_todo_reason)
            ?? "ok $num_of_tests_run - $unescaped-prefix$desc$subtest_todo_reason"
            !! "ok $num_of_tests_run - $unescaped-prefix$desc";

    $tap ~= ("\n$indents# t=" ~ ceiling(($time_after - $time_before)*1_000_000))
        if nqp::iseq_i($raku_test_times,1);

    $output.say: $tap;

    unless $cond {
        my $caller;
        # sub proclaim is not called directly, so 2 is minimum level
        my int $level = 1;

        repeat {
            $caller = callframe(++$level);
        } while $?FILE.ends-with($caller.file);

        # initial level for reporting
        my $tester = $caller;

        repeat {
            ($caller = callframe($level++)) or last;
            my \code := $caller.code;
            $tester = callframe($level)  # the next one should be reported
                if nqp::can(code,'is-test-assertion');  # must use nqp
        } until !$caller.file || $caller.file.ends-with('.nqp');

        # the final place we want to report from
        _diag $desc
          ?? "Failed test '$desc'\nat $tester.file() line $tester.line()"
          !! "Failed test at $tester.file() line $tester.line()";
    }

    # must clear this between tests
    $todo_reason = ''
        if $todo_upto_test_num == $num_of_tests_run
            and nqp::iseq_i($die_on_fail,0);

    $cond
}

sub done-testing(-->Bool:D) is export {
    my $return = True;
    _init_io() unless $output;
    $done_testing_has_been_run = 1;

    if nqp::iseq_i($no_plan,1) {
        $num_of_tests_planned = $num_of_tests_run;
        $output.say: $indents ~ "1..$num_of_tests_planned";
    }

    if ($num_of_tests_planned or $num_of_tests_run)
        && ($num_of_tests_planned != $num_of_tests_run) {
        _diag("You planned $num_of_tests_planned test"
            ~ ($num_of_tests_planned == 1 ?? '' !! 's')
            ~ ", but ran $num_of_tests_run");
        $return = False;
    }
    if $num_of_tests_failed && ! $subtest_todo_reason {
        _diag("You failed $num_of_tests_failed test"
            ~ ($num_of_tests_failed == 1 ?? '' !! 's')
            ~ " of $num_of_tests_run");
        $return = False;
    }
    $return;
}

sub _init_vars {
    $subtest_callable_type = Mu;
    $num_of_tests_run     = 0;
    $num_of_tests_failed  = 0;
    $todo_upto_test_num   = 0;
    $todo_reason          = '';
    $num_of_tests_planned = Any;
    $no_plan              = 1;
    $time_before          = 0;
    $time_after           = 0;
    $done_testing_has_been_run = 0;
    $pseudo_fails         = 0;
    $subtest_todo_reason  = '';
}

sub _push_vars {
    @vars.push: item [
      $subtest_callable_type,
      $num_of_tests_run,
      $num_of_tests_failed,
      $todo_upto_test_num,
      $todo_reason,
      $num_of_tests_planned,
      $no_plan,
      $time_before,
      $time_after,
      $done_testing_has_been_run,
      $pseudo_fails,
      $subtest_todo_reason,
    ];
}

sub _pop_vars {
    (
      $subtest_callable_type,
      $num_of_tests_run,
      $num_of_tests_failed,
      $todo_upto_test_num,
      $todo_reason,
      $num_of_tests_planned,
      $no_plan,
      $time_before,
      $time_after,
      $done_testing_has_been_run,
      $pseudo_fails,
      $subtest_todo_reason,
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

You can also check the documentation about testing in Raku online on:

  https://doc.raku.org/language/testing

=end pod

# vim: expandtab shiftwidth=4
