use lib <t/packages/Test-Helpers>;
use Test::Helpers;
use Test;

plan 1;

# Checking a new multi candidate against the others for an equivalent signature
# runs each parameter type's ACCEPTS. A parameter type whose ACCEPTS comes from
# outside the setting is left out, so its ACCEPTS is not run at compile time.

is-run q:to/CODE/,
        my role R { method ACCEPTS($) { note "ran"; True } }
        my constant T = Str but R;
        multi sub f(T   $x) { }
        multi sub f(Int $x) { }
        print "compiled";
        CODE
    :out("compiled"), :err(""),
    'the duplicate-multi check does not run a parameter type ACCEPTS at compile time';

# vim: expandtab shiftwidth=4
