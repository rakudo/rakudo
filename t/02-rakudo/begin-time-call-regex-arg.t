use Test;

# A capturing regex used as a BEGIN-time argument (a trait argument or a
# BEGIN-evaluated call) must compile and reach the callee as a working Regex.
# A capture generates a !__REGEX_CAPTURE__N lexical whose declaration was lost
# when the argument was compiled outside its enclosing scope.

plan 3;

use MONKEY-SEE-NO-EVAL;

is EVAL(q:to/CODE/), '7',
        my @log;
        multi sub trait_mod:<is>(Attribute:D $a, :$validated!) {
            @log.push: ("7X" ~~ $validated[0]) ?? ~$0 !! 'no'
        }
        class C { has $.a is validated(/^(\d)X$/, 'msg'); }
        @log[0]
        CODE
    'a capturing regex in an attribute trait argument compiles and matches';

is EVAL(q:to/CODE/), '7',
        sub id($x, $y) { $x }
        my $rx = BEGIN id(/^(\d)X$/, 'msg');
        ("7X" ~~ $rx) ?? ~$0 !! 'no'
        CODE
    'a capturing regex in a BEGIN-evaluated call compiles and matches';

is EVAL(q:to/CODE/), '7',
        my @log;
        multi sub trait_mod:<is>(Attribute:D $a, :$validated!) {
            @log.push: ("7X" ~~ $validated) ?? ~$<n> !! 'no'
        }
        class D { has $.a is validated(/^$<n>=(\d)X$/); }
        @log[0]
        CODE
    'a named-capture regex in a trait argument compiles and matches';

# vim: expandtab shiftwidth=4
