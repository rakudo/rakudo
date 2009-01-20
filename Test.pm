# Copyright (C) 2007, The Perl Foundation.
# $Id$

## This is a temporary Test.pm to get us started until we get pugs's Test.pm
## working. It's shamelessly stolen & adapted from MiniPerl6 in the pugs repo.

# globals to keep track of our tests
our $num_of_tests_run     = 0;
our $num_of_tests_failed  = 0;
our $todo_upto_test_num   = 0;
our $todo_reason          = '';
our $die_on_fail          = 0;
our $num_of_tests_planned;

our $*WARNINGS = 0;

# for running the test suite multiple times in the same process
our $testing_started;


## test functions

# Compare numeric values with approximation
sub approx ($x, $y) {
    my $epsilon = 0.00001;
    my $diff = abs($x - $y);
    ($diff < $epsilon);
}

sub plan($number_of_tests) is export() {
    $testing_started      = 1;
    $num_of_tests_planned = $number_of_tests;

    say '1..' ~ $number_of_tests;
}

sub die_on_fail() is export() {
    $die_on_fail = 1;
}

sub pass($desc) is export() {
    proclaim(1, $desc);
}

sub fail($desc) is export() {
    proclaim(0, $desc);
}

sub ok(Object $passed, $desc='') is export() {
    my $diagnostics = diag_bool_true($passed);
    proclaim($passed, $desc, $diagnostics);
}

sub nok(Object $passed, $desc='') is export() {
    my $diagnostics = diag_bool_false($passed);
    proclaim(!$passed, $desc, $diagnostics);
}

sub is(Object $have, Object $want, $desc='') is export() {
    my $passed = $have eq $want;
    proclaim($passed, $desc, diag_eq($passed, $have, $want));
}

sub isnt(Object $have, Object $want, $desc='') is export() {
    my $passed = !($have eq $want);
    proclaim($passed, $desc, diag_neq($passed, $have, $want));
}

sub is_approx(Object $have, Object $want, $desc='') is export() {
    my $passed = abs($have - $want) <= 0.00001;
    proclaim($passed, $desc, diag_approx($passed, $have, $want));
}

sub todo($reason, $count=1) is export() {
    $todo_upto_test_num = $num_of_tests_run + $count;
    $todo_reason = '# TODO ' ~ $reason;
}

multi sub skip()                is export() { proclaim(1, "# SKIP"); }
multi sub skip($reason)         is export() { proclaim(1, "# SKIP " ~ $reason); }
multi sub skip($count, $reason) is export() {
    for 1..$count {
        proclaim(1, "# SKIP " ~ $reason);
    }
}

multi sub skip_rest() is export() {
    skip($num_of_tests_planned - $num_of_tests_run, "");
}

multi sub skip_rest($reason) is export() {
    skip($num_of_tests_planned - $num_of_tests_run, $reason);
}

sub diag($message) is export() { say '# '~$message; }


multi sub flunk($reason) is export() { proclaim(0, "flunk $reason")}


multi sub isa_ok($var,$type) is export() {
    ok($var.isa($type), "The object is-a '$type'");
}
multi sub isa_ok($var,$type, $msg) is export() { ok($var.isa($type), $msg); }

multi sub dies_ok($closure, $reason) is export() {
    try {
        $closure();
    }
    proclaim((defined $!), $reason);
}
multi sub dies_ok($closure) is export() {
    dies_ok($closure, '');
}

multi sub lives_ok($closure, $reason) is export() {
    try {
        $closure();
    }
    proclaim((not defined $!), $reason);
}
multi sub lives_ok($closure) is export() {
    lives_ok($closure, '');
}

multi sub eval_dies_ok($code, $reason) is export() {
    proclaim((defined eval_exception($code)), $reason);
}
multi sub eval_dies_ok($code) is export() {
    eval_dies_ok($code, '');
}

multi sub eval_lives_ok($code, $reason) is export() {
    proclaim((not defined eval_exception($code)), $reason);
}
multi sub eval_lives_ok($code) is export() {
    eval_lives_ok($code, '');
}


sub is_deeply($have, $want, $reason='') {
    my $passed = _is_deeply( $have, $want );
    my $diagnostics = diag_eq($passed, $have, $want);
    proclaim($passed, $reason, $diagnostics);
}

sub _is_deeply( $this, $that) {

    if $this ~~ List && $that ~~ List {
        return if +$this.values != +$that.values;
        for $this Z $that -> $a, $b {
            return if ! _is_deeply( $a, $b );
        }
        return True;
    }
    elsif $this ~~ Hash && $that ~~ Hash {
        return if +$this.keys != +$that.keys;
        for $this.keys.sort Z $that.keys.sort -> $a, $b {
            return if $a ne $b;
            return if ! _is_deeply( $this{$a}, $that{$b} );
        }
        return True;
    }
    elsif $this ~~ Str | Num | Int && $that ~~ Str | Num | Int {
        return $this eq $that;
    }
    elsif $this ~~ Pair && $that ~~ Pair {
        return $this.key eq $that.key
               && _is_deeply( $this.value, $this.value );
    }
    elsif $this ~~ undef && $that ~~ undef && $this.WHAT eq $that.WHAT {
        return True;
    }

    return;
}


## 'private' subs

sub diag_bool_true($passed) {
    # Workaround for: Method 'perl' not found for invocant (various classes,
    # including some anonymous, so can't just make a list).
    my $have;
    try { $have = $passed.perl; CATCH { $have = $passed } }
    return $passed
        ?? ''
        !! "# Expected a true value.\n# have: {$have}";
}

sub diag_bool_false($passed) {
    return $passed
        ?? "# Expected a false value.\n# have: {$passed.perl}"
        !! '';
}

sub diag_eq($passed, $have, $want) {
    # Workaround for: Method 'perl' not found for invocant (various classes,
    # including some anonymous, so can't just make a list).
    my $x_have;
    try { $x_have = $passed.perl; CATCH { $x_have = $passed } }
    return $passed ?? '' !! "# have: {$x_have}\n# want: {$want.perl}";
}

sub diag_neq($passed, $have, $want) {
    return $passed ?? '' !! "# Expected different values\n# have: {$have.perl}\n# want: {$want.perl}";
}

sub diag_approx($passed, $have, $want) {
    return $passed ?? '' !! "# Expected approximately the same values\n# have: {$have.perl}\n# want: {$want.perl}";
}

sub eval_exception($code) {
    my $eval_exception;
    try { eval ($code); $eval_exception = $! }
    $eval_exception // $!;
}

sub proclaim($passed, $desc, $diagnostics='') {
    $testing_started  = 1;
    $num_of_tests_run = $num_of_tests_run + 1;

    unless $passed {
        print "not ";
        $num_of_tests_failed = $num_of_tests_failed + 1
            unless  $num_of_tests_run <= $todo_upto_test_num;
    }
    print "ok ", $num_of_tests_run, " - ", $desc;
    if $todo_reason and $num_of_tests_run <= $todo_upto_test_num {
        print $todo_reason;
    }
    print "\n";
    say $diagnostics if $diagnostics;
    if !$passed && $die_on_fail && !$todo_reason {
        die "Test failed.  Stopping test.";
    }
    $todo_reason = '';   # must reset between tests
    return $passed;
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
