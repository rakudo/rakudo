module Test-0.0.6;

### CONSTANTS

# some options available through the environment
$Test::ALWAYS_CALLER = %*ENV<TEST_ALWAYS_CALLER>;

### GLOBALS

# globals to keep track of our tests
$Test::num_of_tests_run    = 0;
$Test::num_of_tests_failed = 0;
$Test::num_of_tests_badpass = 0;
$Test::num_of_tests_planned;

# a Junction to hold our FORCE_TODO tests
$Test::force_todo_test_junction;

# for running the test suite multiple times in the same process
$Test::testing_started = 1;

### FUNCTIONS

## plan

sub plan (Int $number_of_tests) returns Void is export {
    $Test::testing_started = 1;
    $Test::num_of_tests_planned = $number_of_tests;
    say "1..$number_of_tests";
}

sub force_todo (*@todo_tests) returns Void is export {
     $Test::force_todo_test_junction = join ' ', '', @todo_tests, '';
}

## ok

sub ok (Bool $cond, Str $desc?, :$todo, :$depends) returns Bool is export {
    Test::proclaim($cond, $desc, $todo, :$depends);
}

## is

sub is (Str $got, Str $expected, Str $desc?, :$todo, :$depends) returns Bool is export {
    my $test := $got eq $expected;
    Test::proclaim($test, $desc, $todo, $got, $expected, $depends);
}

## is_deeply
sub is_deeply(Any $got, Any $expected, Str $desc?, :$todo, :$depends) returns Bool is export {
    # hack for now
    my $got_perl = $got.perl;
    my $expected_perl = $expected.perl;
    my $test := ($got_perl eq $expected_perl);
    Test::proclaim($test, $desc, $todo, $got_perl, $expected_perl, $depends);
}


## isnt

sub isnt (Str $got, Str $expected, Str $desc?, :$todo, :$depends) returns Bool is export {
    my $test := not($got eq $expected);
    Test::proclaim($test, "Should not match: $desc", $todo, $got, $expected, $depends, :negate);
}

## like

sub like (Str $got, Rule $expected, Str $desc?, :$todo, :$depends) returns Bool is export {
    my $test := $got ~~ $expected;
    Test::proclaim($test, $desc, $todo, $got, $expected, $depends);
}

## unlike

sub unlike (Str $got, Rule $expected, Str $desc?, :$todo, :$depends) returns Bool is export {
    my $test := not($got ~~ $expected);
    Test::proclaim($test, $desc, $todo, $got, $expected, $depends, :negate);
}

## eval_ok

sub eval_ok (Str $code, Str $desc?, :$todo, :$depends) returns Bool is export {
    my $result := eval $code;
    if (defined $!) {
        Test::proclaim(undef, $desc, $todo, "eval was fatal: $!", :$depends);
    }
    else {
        #diag "'$desc' was non-fatal and maybe shouldn't use eval_ok()";
        &Test::ok.goto($result, $desc, :$todo, :$depends);
    }
}

## eval_is

sub eval_is (Str $code, Str $expected, Str $desc?, :$todo, :$depends) returns Bool is export {
    my $result := eval $code;
    if (defined $!) {
        Test::proclaim(undef, $desc, $todo, "eval was fatal: $!", $expected, $depends);
    }
    else {
        #diag "'$desc' was non-fatal and maybe shouldn't use eval_is()";
        &Test::is.goto($result, $expected, $desc, :$todo, :$depends);
    }
}

## cmp_ok

sub cmp_ok (Str $got, Code &compare_func, Str $expected, Str $desc?, :$todo, :$depends) returns Bool is export {
    my $test := compare_func($got, $expected);
    Test::proclaim($test, $desc, $todo, $got, "&compare_func.name() $expected", $depends);
}

## isa_ok

sub isa_ok (Any|Junction|Pair $ref is rw, Str $expected_type, Str $desc?, :$todo, :$depends) returns Bool is export {
    my $out := defined($desc) ?? $desc !! "The object is-a '$expected_type'";
    my $test := $ref.isa($expected_type);
    Test::proclaim($test, $out, $todo, $ref.ref, $expected_type, $depends);
}

## use_ok

sub use_ok (Str $module, :$todo, :$depends) is export {
    my $caller = caller().package;

    eval "package $caller; require $module";

    #try {
    #    &::($module)::import.goto();
    #};

    if ($!) {
        Test::proclaim(undef, "require $module;", $todo, "Import error when loading $module: $!", :$depends);
    }
    else {
        &Test::ok.goto(1, "$module imported OK", :$todo, :$depends);
    }
}

## throws ok

sub throws_ok (Code &code, Any $match, Str $desc?, :$todo, :$depends) returns Bool is export {
    try { code() };
    if ($!) {
        &Test::ok.goto($! ~~ $match, $desc, :$todo, :$depends);
    }
    else {
        Test::proclaim(undef, $desc, $todo, "No exception thrown", :$depends);
    }
}

## dies_ok

sub dies_ok (Code &code, Str $desc?, :$todo, :$depends) returns Bool is export {
    try { code() };
    if ($!) {
        &Test::ok.goto(1, $desc, :$todo);
    }
    else {
        Test::proclaim(undef, $desc, $todo, "No exception thrown", :$depends);
    }
}

## lives ok

sub lives_ok (Code &code, Str $desc?, :$todo, :$depends) returns Bool is export {
    try { code() };
    if ($!) {
        Test::proclaim(undef, $desc, $todo, "An exception was thrown : $!", :$depends);
    }
    else {
        &Test::ok.goto(1, $desc, :$todo, :$depends);
    }
}

## misc. test utilities

multi sub skip (Str $reason?, :$depends) returns Bool is export {
    Test::proclaim(1, "", "skip $reason", :$depends);
}

multi sub skip (Int $count, Str $reason, :$depends) returns Bool is export {
    for (1 .. $count) {
        # Hack -- PIL2JS doesn't support multisubs correctly yet
        if $*OS eq "browser" {
            Test::proclaim(1, "", "skip $reason", :$depends);
        } else {
            Test::skip $reason, :$depends;
        }
    }
}

sub skip_rest (Str $reason?, :$depends) returns Bool is export {
    Test::skip($Test::num_of_tests_planned - $Test::num_of_tests_run, $reason // "", :$depends);
}

sub pass (Str $desc?) returns Bool is export {
    Test::proclaim(1, $desc);
}

sub flunk (Str $desc?, :$todo, :$depends) returns Bool is export {
    Test::proclaim(0, $desc, $todo, :$depends);
}

sub diag (Str $diag) is export {
    for (split("\n", $diag)) -> $line {
        say "# $line";
    }
}

## 'private' subs

sub proclaim (Bool $cond, Str $desc? is copy, $todo?, Str $got?, Str $expected?, $depends?, $negate?) returns Bool {
    $Test::testing_started = 1;
    $Test::num_of_tests_run++;

    # $context is now the raw TODO, so we have to check it
    my $context;

    # Check if we have to forcetodo this test
    # because we're preparing for a release.
    $context = "TODO for release"
        if index($Test::force_todo_test_junction, ' '~$Test::num_of_tests_run~' ') >= 0;

    if $todo {
        if (substr($todo, 0, 4) eq 'skip') {
            $context = $todo;
        }
        else {
            $context =  "TODO" ~ ($todo.isa('Str') ?? " $todo" !! '');
            if ( $cond ) {
                $Test::num_of_tests_badpass ++;
            }
        }
    }

    if ( $depends ) {
        $context ~= " (depends on $depends working)";
    }

    my $out = $desc.defined ?? " - $desc" !! "";
    $out = "$out <pos:$?CALLER::CALLER::POSITION>" if $Test::ALWAYS_CALLER;
    # message like "test #1 # TODO" confuse the harness. Escape desc #s.
    $out ~~ s:P5:g/#/\\#/;

    my $context_out = $context.defined ?? " # $context" !! "";

    print "not " unless $cond;
    say "ok ", $Test::num_of_tests_run, $out, $context_out;

    Test::report_failure($context, $got, $expected, $negate) unless $cond;

    return $cond;
}

sub report_failure (Str $todo?, Str $got?, Str $expected?, Bool $negate?) returns Bool {
    if ($todo) {
        Test::diag("  Failed ($todo) test ($?CALLER::CALLER::CALLER::POSITION)");
    }
    else {
        Test::diag("  Failed test ($?CALLER::CALLER::CALLER::POSITION)");
        $Test::num_of_tests_failed++;
    }
    my $wanted = $negate ?? "Unwanted" !! "Expected";

    # As PIL2JS doesn't support junctions yet, skip the junction part when
    # running under PIL2JS.
    if (index('&Test::is &Test::isnt &Test::cmp_ok &Test::eval_is &Test::isa_ok &Test::is_deeply &Test::todo_is &Test::todo_isnt &Test::todo_cmp_ok &Test::todo_eval_is &Test::todo_isa_ok ', $?CALLER::CALLER::SUBNAME ~ ' ') >= 0) {
        Test::diag("  $wanted: '" ~ ($expected.defined ?? $expected !! "undef") ~ "'");
        Test::diag("    Actual: '" ~ ($got.defined ?? $got !! "undef") ~ "'");
    }
    else {
        Test::diag("    Actual: " ~ ($got.defined ?? $got !! "undef"));
    }
}


sub test_ends {
    return unless $Test::testing_started;
    if (!defined($Test::num_of_tests_planned)) {
        say("1..$Test::num_of_tests_run");
    }
    elsif ($Test::num_of_tests_planned != $Test::num_of_tests_run) {
        $*ERR.say("# Looks like you planned $Test::num_of_tests_planned tests, but ran $Test::num_of_tests_run");
    }

    if ($Test::num_of_tests_failed) {
        $*ERR.say("# Looks like you failed $Test::num_of_tests_failed tests of $Test::num_of_tests_run");
    }
    if ($Test::num_of_tests_badpass) {
        $*ERR.say("# Looks like $Test::num_of_tests_badpass tests of $Test::num_of_tests_run passed unexpectedly");
    }
    $Test::num_of_tests_run    = 0;
    $Test::num_of_tests_failed = 0;
    $Test::num_of_tests_badpass = 0;
    $Test::num_of_tests_planned = undef;
    $Test::force_todo_test_junction = undef;
    $Test::testing_started = 0;
}

END { Test::test_ends() }

=kwid

= NAME

Test - Test support module for perl6

= SYNOPSIS

  use v6;
  require Test;

  plan 10;
  force_todo(1, 3 .. 5, 9);

  use_ok('Some::Module');
  use_ok('Some::Other::Module', todo => 1);

  ok(2 + 2 == 4, '2 and 2 make 4');
  is(2 + 2, 4, '2 and 2 make 4');
  isa_ok([1, 2, 3], 'List');

  ok(2 + 2 == 5, '2 and 2 make 5', :todo(1));
  is(2 + 2, 5, desc => '2 and 2 make 5', todo => 1);
  isa_ok({'one' => 1}, 'Hash', :todo(1));

  use_ok('My::Module');

  pass('This test passed');
  flunk('This test failed');

  skip('skip this test for now');

  flunk('this fails, but might work soon', :todo(1));

  diag('some misc comments and documentation');

= DESCRIPTION

This module was built to facilitate the Pugs test suite. It has the
distinction of being the very first module written for Pugs.

It provides a simple set of common test utility functions, and is
an implementation of the TAP protocol.

This module, like Pugs, is a work in progress. As new features are
added to Pugs, new test functions will be defined to facilitate the
testing of those features. For more information see the FUTURE PLANS
section of this document.

= FUNCTIONS

- `plan (Int $number_of_tests) returns Void`

All tests need a plan. A plan is simply the number of tests which are
expected to run. This should be specified at the very top of your tests.

- `force_todo (*@todo_tests) returns Void`

If you have some tests which you would like to force into being TODO tests
then you can pass them through this function. This is primarily a release
tool, but can be useful in other contexts as well.

== Testing Functions

- `use_ok (Str $module, Bool :$todo, Str :$depends) returns Bool`

*NOTE:* This function currently uses `require()` since Pugs does not yet have
a proper `use()` builtin.

- `ok (Bool $cond, Str $desc?, Bool :$todo, Str :$depends) returns Bool`

- `is (Str $got, Str $expected, Str $desc?, Bool :$todo, Str :$depends) returns Bool`

- `isnt (Str $got, Str $expected, Str $desc?, Bool :$todo, Str :$depends) returns Bool`

- `like (Str $got, Rule $expected, Str $desc?, Bool :$todo, Str :$depends) returns Bool is export`
- `unlike (Str $got, Rule $expected, Str $desc?, Bool :$todo, Str :$depends) returns Bool is export`

These functions should work with most reg-exps, but given that they are still a
somewhat experimental feature in Pugs, it is suggested you don't try anything
too funky.

- `cmp_ok (Str $got, Code &compare_func, Str $expected, Str $desc?, Bool :$todo, Str :$depends) returns Bool`

This function will compare `$got` and `$expected` using `&compare_func`. This will
eventually allow Test::More-style cmp_ok() though the following syntax:

  cmp_ok('test', &infix:<gt>, 'me', '... testing gt on two strings');

However the `&infix:<gt>` is currently not implemented, so you will have to wait
a little while. Until then, you can just write your own functions like this:

  cmp_ok('test', sub ($a, $b) { ?($a gt $b) }, 'me', '... testing gt on two strings');

- `isa_ok ($ref, Str $expected_type, Str $desc?, Bool :$todo, Str :$depends) returns Bool`

This function currently on checks with ref() since we do not yet have
object support. Once object support is created, we will add it here, and
maintain backwards compatibility as well.

- `eval_ok (Str $code, Str $desc?, Bool :$todo, Str :$depends) returns Bool`

- `eval_is (Str $code, Str $expected, Str $desc?, Bool :$todo, Str :$depends) returns Bool`

These functions will eval a code snippet, and then pass the result to is or ok
on success, or report that the eval was not successful on failure.

- `throws_ok (Code &code, Any $expected, Str $desc?, Bool :$todo, Str :$depends) returns Bool`

This function takes a block of code and runs it. It then smart-matches (`~~`) any `$!`
value with the `$expected` value.

- `dies_ok (Code &code, Str $desc?, Bool :$todo, Str :$depends) returns Bool`

- `lives_ok (Code &code, Str $desc?, Bool :$todo, Str :$depends) returns Bool`

These functions both take blocks of code, run the code, and test whether they live or die.

=== A Note about TODO-ing tests

Sometimes a test is broken because something is not implemented yet. So
in order to still allow that to be tested, and those tests to knowingly
fail, we provide the `:todo(1)` named parameter for all these  functions.

The `:depends("string")` parameter to most of the functions is a way
to provide a comment that refers to another file or test which must be
made to pass before this test can pass (or before an implementation
could be started).  This is most useful when writing modules and you
find there is some language feature missing, or core bug that needs to
be sorted out before you can continue.

It is also possible to use the `force_todo()` function to do large scale
TODO-ing of tests.

== Misc. Functions

- `skip (Str $reason?) returns Bool`
- `skip (Int $count, Str $reason?) returns Bool`

If for some reason a test is to be skipped, you can use this
function to do so.

- `pass (Str $desc?) returns Bool`

Sometimes what you need to test does not fit into one of the standard
testing functions. In that case, you can use the rather blunt pass()
functions and its compliment the flunk() function.

- `flunk (Str $desc?, Bool :$todo) returns Bool`

This is the opposite of pass()

- `diag (Str $diag)`

This will print each string with a '#' character appended to it, this is
ignored by the TAP protocol.

= FUTURE PLANS

This module is still a work in progress. As Pugs grows, so will it's
testing needs. This module will be the code support for those needs. The
following is a list of future features planned for this module.

- better error handling for cmp_ok

The error handling capabilities need to be expanded more to handle the
error reporting needs of the cmp_ok() function.

- is_deeply

Once nested data structures are implemented, we will need an easy way
to test them. So we will implement the Test::More function is_deeply.
The plan currently is to implement this as a mutually recursive multi-
sub which will be able to handle structures of arbitrary depth and of
an arbitrary type. The function signatures will likely look something
like this:

  multi sub is_deeply (Array @got, Array @expected, Str $desc?) returns Bool;
  multi sub is_deeply (List  $got, List  $expected, Str $desc?) returns Bool;
  multi sub is_deeply (Hash  %got, Hash  %expected, Str $desc?) returns Bool;
  multi sub is_deeply (Pair  $got, Pair  $expected, Str $desc?) returns Bool;

Because these functions will be mutually recursive, they will easily be
able handle arbitrarily complex data structures automatically (at least
that is what I hope).

= ENVIRONMENT

Setting the environment variable TEST_ALWAYS_CALLER to force Test.pm to always
append the caller information to the test's `$desc`.

= SEE ALSO

The Perl 5 Test modules

- Test

- Test::More

Information about the TAP protocol can be found in the Test::Harness
distribution.

= AUTHORS

Audrey Tang <autrijus@autrijus.org>

Benjamin Smith

Norman Nunley

Steve Peters

Stevan Little <stevan@iinteractive.com>

Brian Ingerson <ingy@cpan.org>

Jesse Vincent <jesse@bestpractical.com>

Yuval Kogman <nothingmuch@woobling.org>

Darren Duncan <perl@DarrenDuncan.net>

Nathan Gray <kolibrie@graystudios.org>

Max Maischein <corion@cpan.org>

Ingo Blechschmidt <iblech@web.de>

Gaal Yahas <gaal@forum2.org>

= COPYRIGHT

Copyright (c) 2005, 2006. Audrey Tang. All rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See http://www.perl.com/perl/misc/Artistic.html

=cut
