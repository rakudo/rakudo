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
is EVAL(q[my class NativeValues does Associative[int] { method AT-KEY($) { 42 } }; sub f(int %h) { %h<a> }; f(NativeValues.new)]), 42,
    'a hash parameter takes a native value type';

# vim: expandtab shiftwidth=4
