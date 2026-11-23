use Test;

plan 6;

use MONKEY-SEE-NO-EVAL;

throws-like { EVAL q[my int %h] }, X::Comp::NYI,
    feature => 'native value types for hashes',
    'a hash refuses a native value type';
throws-like { EVAL q[my int %h{Str}] }, X::Comp::NYI,
    feature => 'native value types for hashes',
    'a keyed hash refuses a native value type';
throws-like { EVAL q[class { has num %.h }] }, X::Comp::NYI,
    feature => 'native value types for hashes',
    'a hash attribute refuses a native value type';
throws-like { EVAL q[my %h of int] }, X::Comp::NYI,
    feature => 'native value types for hashes',
    'a hash refuses a native value type given by an of trait';
throws-like { EVAL q[my (int %h)] }, X::Comp::NYI,
    feature => 'native value types for hashes',
    'a hash declared in a signature declarator refuses a native value type';
throws-like { EVAL q[sub a(int %h) { }] }, X::Comp::NYI,
    feature => 'native value types for hashes',
    'a hash declared with a native value type as a parameter';

# vim: expandtab shiftwidth=4
