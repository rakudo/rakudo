use v6.e.PREVIEW;
use Test;

plan 13;

# From 6.e on a concrete Match on the right of a smartmatch is an ordinary
# identity comparison, as for any other object without its own ACCEPTS,
# rather than returning the Match as-is.

my $m = 'foo' ~~ /o+/;
is ('bar' ~~ $m), False, 'smartmatch of a non-identical topic against a Match is False';
is ($m ~~ $m),    True,  'smartmatch of a Match against itself is True';
is ('bar' !~~ $m), True,  'negated smartmatch of a non-identical topic against a Match is True';
is ($m !~~ $m),    False, 'negated smartmatch of a Match against itself is False';

my $other = 'foo' ~~ /o+/;
is ($other ~~ $m), False, 'an equivalent but distinct Match is not identical';

# The typeobject keeps its usual typecheck path.
is ($m ~~ Match),    True,  'smartmatch against the Match typeobject stays a typecheck';
is ('bar' ~~ Match), False, 'a non-Match topic does not typecheck against the Match typeobject';

# `when` compiles to a direct ACCEPTS call rather than the infix.
given $m {
    when $m { pass 'when against the identical Match fires' }
    default { flunk 'when against the identical Match fires' }
}
given 'x' {
    when $m { flunk 'when against a different topic falls through to default' }
    default { pass 'when against a different topic falls through to default' }
}

# A Junction topic threads the identity comparison over its eigenstates.
is (any('a', $m) ~~ $m),  True,  'any-Junction holding the Match as an eigenstate is True';
is (any('a', 'b') ~~ $m), False, 'any-Junction without the Match is False';
is (all('a', $m) !~~ $m), True,  'negated all-Junction with a non-identical eigenstate is True';

# Matching against a regex is not an identity comparison on the fresh Match.
isa-ok ('foo' ~~ m/o+/), Match, 'smartmatch against m// still returns the Match';

# vim: expandtab shiftwidth=4
