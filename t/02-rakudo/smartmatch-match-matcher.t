use Test;

plan 9;

# A concrete Match on the right of a smartmatch is a match result already,
# so the operator returns it as-is, the way matching against a regex returns
# the Match and the way the raku-smartmatch dispatcher behaves.

my $m = 'foo' ~~ /o+/;
my $r = 'bar' ~~ $m;
isa-ok $r, Match, 'smartmatch against a Match returns a Match';
ok $r === $m, 'the matcher itself is the result';
is ('bar' !~~ $m), False, 'negated smartmatch against a successful Match';

my $empty = Match.new;
ok ('bar' ~~ $empty) === $empty, 'a constructed Match is returned as-is too';
is ('bar' !~~ $empty), False, 'a constructed Match is truthy, so its negated smartmatch is False';

# A Junction topic still matches over its eigenstates, collapsing to a Bool
# from each eigenstate's smartmatch against the Match.
isa-ok (any('a', 'b') ~~ $m), Bool, 'Junction topic against a Match collapses to Bool';
is (any('a', 'b') ~~ $m),  True,  'any-Junction against a successful Match';
is (none('a', 'b') ~~ $m), False, 'none-Junction against a successful Match';
is (all('a', 'b') !~~ $m), False, 'negated all-Junction against a successful Match';

# vim: expandtab shiftwidth=4
