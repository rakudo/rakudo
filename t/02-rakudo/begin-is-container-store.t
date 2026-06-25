use Test;

plan 3;

# A `my %h is SomeType = ...` evaluated in a BEGIN must vivify the container
# before the initializing STORE. At BEGIN the declaration's own vivification
# has not run yet, so the container is still a type object, and an immutable
# Associative (Map, Set, ...) whose STORE needs a defined invocant would fail.

is do { my $x := BEGIN my %h is Map = (a => 1, b => 2); $x<a> }, 1,
    'my %h is Map initialized in a BEGIN';

is do { my $x := BEGIN my %h is Set = <a b c>; $x<b> }, True,
    'my %h is Set initialized in a BEGIN';

# Outside a BEGIN the same declaration keeps working.
is do { my %h is Map = (a => 1); %h<a> }, 1,
    'my %h is Map initialized outside a BEGIN';

# vim: expandtab shiftwidth=4
