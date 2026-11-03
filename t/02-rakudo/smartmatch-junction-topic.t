use Test;

plan 31;

# A Junction on the left of a smartmatch matches over its eigenstates, even
# against a matcher (such as a Hash) whose ACCEPTS sees the whole topic rather
# than distributing the Junction itself. Acme::BaseCJK relies on this in
# `subset ... where .comb.all ~~ %table`.

my %h = a => 1, b => 1;
my constant %con = (a => 1, b => 1);

is (all('a', 'b')  ~~ %h),   True,  'all-Junction against a Hash, every key present';
is (all('a', 'x')  ~~ %h),   False, 'all-Junction against a Hash, a key missing';
is (any('x', 'b')  ~~ %h),   True,  'any-Junction against a Hash, one key present';
is (none('x', 'y') ~~ %h),   True,  'none-Junction against a Hash, no key present';
is (none('a', 'y') ~~ %h),   False, 'none-Junction against a Hash, a key present';
is (one('a', 'x')  ~~ %h),   True,  'one-Junction against a Hash, exactly one key present';
is (all('a', 'b')  ~~ %con), True,  'all-Junction against a Hash constant';

# Negated smartmatch distributes too.
is (all('a', 'b') !~~ %h), False, 'negated all-Junction against a Hash, every key present';
is (all('x', 'y') !~~ %h), True,  'negated all-Junction against a Hash, no key present';

# Type object matcher held in a variable.
my $t = Int;
is (any(1, 's') ~~ $t), True, 'any-Junction against a type held in a variable';

# A Junction on both sides.
is (any('a', 'z') ~~ any('a', 'b')), True, 'Junction on both sides of a smartmatch';

# A variable holding a regex on the right still returns the Match and sets $/.
my $rx = /f(o+)/;
my $r = 'foo' ~~ $rx;
isa-ok $r, Match, 'a regex held in a variable returns a Match';
is ~$/[0], 'oo', 'a regex held in a variable sets $/';

# The negated smartmatch sets $/ too, so captures from the match remain
# available after a successful `nok`-style check.
$/ = Nil;
is ('foo' !~~ $rx), False, 'negated smartmatch against a matching regex variable';
is ~$/[0], 'oo', 'the negated smartmatch still sets $/';

# The pattern from Acme::BaseCJK: a subset whose where-clause threads a
# Junction over a constant Map.
my constant %T = Map.new: ('a' .. 'e').map({ $_ => $++ });
subset S of Str:D where .comb.all ~~ %T;
ok  'abc' ~~ S, 'subset where-clause threading all() over a Map accepts a member';
nok 'xyz' ~~ S, 'subset where-clause rejects a non-member';

# A matcher class with its own ACCEPTS receives the whole Junction rather than
# having it distributed for it.
my Mu $topic-seen;
my class JunctionAware {
    multi method ACCEPTS(Junction:D \topic) { $topic-seen = topic.WHAT; True  }
    multi method ACCEPTS(Mu \topic)         { $topic-seen = topic.WHAT; False }
}
my $aware = JunctionAware.new;
my $j = any(1, 2);
is ($j ~~ $aware), True, 'a matcher with its own Junction ACCEPTS candidate decides the match';
ok $topic-seen === Junction, 'that ACCEPTS candidate received the whole Junction';
is ($j !~~ $aware), False, 'negated smartmatch defers to the custom ACCEPTS too';

my class WholeTopic {
    method ACCEPTS(Mu \topic) { $topic-seen = topic.WHAT; True }
}
$topic-seen = Nil;
is ($j ~~ WholeTopic.new), True, 'a non-multi custom ACCEPTS decides the match';
ok $topic-seen === Junction, 'a non-multi custom ACCEPTS received the whole Junction';

# A Junction topic against a regex held in a variable: Regex.ACCEPTS
# autothreads the Junction itself and returns a Junction of match results.
my $rxa = /a/;
my $jm = any('cat', 'dog');
my $matched = $jm ~~ $rxa;
isa-ok $matched, Junction, 'Junction topic against a regex variable returns a Junction';
ok $matched.so, 'the match Junction collapses to True when an eigenstate matches';
my $jn = any('xxx', 'yyy');
nok ($jn ~~ $rxa).so, 'the match Junction collapses to False when no eigenstate matches';
is ($jn !~~ $rxa), True, 'negated smartmatch of a Junction against a regex variable';

# A complex right side (not a variable or constant) matches the same way as
# a variable one.
is (all('a', 'b') ~~ %h.Map), True, 'all-Junction against a complex expression yielding a Map';
$topic-seen = Nil;
my %matchers = m => JunctionAware.new;
is ($j ~~ %matchers<m>), True, 'a custom ACCEPTS matcher from a complex expression decides the match';
ok $topic-seen === Junction, 'that matcher received the whole Junction';
my %rxs = k => /f(o+)/;
my $cm = 'foo' ~~ %rxs<k>;
isa-ok $cm, Match, 'a regex from a complex expression returns a Match';
is ~$/[0], 'oo', 'a regex from a complex expression sets $/';

# vim: expandtab shiftwidth=4
