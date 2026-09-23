use Test;

# Code compiled at BEGIN or CHECK time must reach a routine declared in
# the unit being compiled when that routine has a native return type or
# the call sits inside a regex.

plan 13;

use MONKEY-SEE-NO-EVAL;

is EVAL(q:to/CODE/), 42,
        sub f(--> int32) { 42 }
        BEGIN f()
        CODE
    'a BEGIN-time call to a routine returning int32';

is EVAL(q:to/CODE/), 42,
        sub f(--> int32) { 42 }
        my $r;
        CHECK $r = f();
        $r
        CODE
    'a CHECK-time call to a routine returning int32';

is EVAL(q:to/CODE/), 42,
        sub f() returns uint32 { 42 }
        BEGIN f()
        CODE
    'a BEGIN-time call to a routine with a returns uint32 trait';

is EVAL(q:to/CODE/), 1e0,
        sub f(--> num64) { 1e0 }
        BEGIN f()
        CODE
    'a BEGIN-time call to a routine returning num64';

is EVAL(q:to/CODE/), 's',
        sub f(--> str) { 's' }
        BEGIN f()
        CODE
    'a BEGIN-time call to a routine returning str';

is EVAL(q:to/CODE/), 1,
        sub f(--> int) { 1 }
        BEGIN { my $c = { f() }; $c() }
        CODE
    'a BEGIN-time call to a native return routine from a nested block';

is EVAL(q:to/CODE/), 1,
        sub f(--> int) { 1 }
        sub g() { f() }
        BEGIN g()
        CODE
    'a native return routine called by a routine that runs at BEGIN time';

is EVAL(q:to/CODE/), 3,
        sub f(int $x --> int) { $x + 1 }
        BEGIN f(f(f(0)))
        CODE
    'nested BEGIN-time calls to a routine returning int';

is EVAL(q:to/CODE/), 'a',
        sub f() { 'a' }
        BEGIN ~('a' ~~ /<{ f() }>/)
        CODE
    'a BEGIN-time call inside a regex interpolation block';

is EVAL(q:to/CODE/), 1,
        sub f() { 1 }
        my $r;
        BEGIN 'a' ~~ /a { $r = f() }/;
        $r
        CODE
    'a BEGIN-time call inside a regex code block';

is EVAL(q:to/CODE/), True,
        sub f() { True }
        BEGIN so 'a' ~~ /a <?{ f() }>/
        CODE
    'a BEGIN-time call inside a regex assertion';

is EVAL(q:to/CODE/), 'a',
        sub f() { 'a' }
        BEGIN ~('a' ~~ /$(f())/)
        CODE
    'a BEGIN-time call inside a regex variable interpolation';

is EVAL(q:to/CODE/), 'a',
        my token t($x) { $x }
        sub f() { 'a' }
        BEGIN ~('a' ~~ /<&t(f())>/)
        CODE
    'a BEGIN-time call in the arguments of a callable subrule';

# vim: expandtab shiftwidth=4
