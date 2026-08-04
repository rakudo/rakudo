use Test;

plan 28;

# https://github.com/rakudo/rakudo/issues/6075
# cmp on collections compares the values they contain, not their
# stringifications

# Seq involving comparisons compare element by element
is (10, 5).Seq cmp (7, 6).Seq, More, 'Seq cmp Seq compares element by element';
is (10, 5)     cmp (7, 6).Seq, More, 'List cmp Seq compares element by element';
is (10, 5).Seq cmp (7, 6),     More, 'Seq cmp List compares element by element';
is (1, "5 6")  cmp (1, 5, 6).Seq, More,
    'Seq comparison no longer coincides with the stringification';
is (1, 2, 3).Seq cmp (1, 2, 3).Seq, Same, 'equal Seqs compare Same';
is (1, 2).Seq cmp (1, 2, 3).Seq, Less, 'shorter Seq with equal prefix is Less';

# a Seq can be compared more than once and survives the comparison
my $seq = (3, 1).Seq;
is $seq cmp (3, 2), Less, 'first comparison of the same Seq';
is $seq cmp (3, 0), More, 'second comparison of the same Seq';
is-deeply $seq.List, (3, 1), 'Seq still has its values after comparisons';
is $seq cmp $seq, Same, 'Seq cmp itself is Same';

# sorting Seqs compares each Seq multiple times
is-deeply ((3, 1).Seq, (2, 0).Seq, (1, 9).Seq, (2, 0).Seq).sort.head.List,
    (1, 9), 'sorting a list of Seqs works';

# laziness is preserved
is (lazy (1, 2, 3)) cmp (1, 2, 3), Same, 'lazy Seq cmp List';
is (1..Inf).Seq cmp (1, 2, 3), More, 'infinite Seq cmp finite List';

# Map cmp is content based and deterministic
my $h1 = {:path(['a']), :value('alpha')};
my $h2 = {:path(['b']), :value('bravo')};
my $h3 = {:path(['c']), :value('charlie')};
is $h1 cmp $h2, Less, 'multi-key Hash cmp by content is Less';
is $h2 cmp $h1, More, 'multi-key Hash cmp by content is More';
is {:a(1), :b(2)} cmp {:a(1), :b(2)}, Same, 'equal Hashes compare Same';
is {:a(10)} cmp {:a(9)}, More, 'Hash cmp compares values as values';
is-deeply ($h3, $h1, $h2).sort.map(*.<path>[0]).list, ('a', 'b', 'c'),
    'sort of multi-key Hashes orders them by content';
is Map.new((:b(2), :a(1))) cmp Map.new((:a(1), :b(2))), Same,
    'Map cmp ignores insertion order';

# QuantHash cmp is content based and deterministic
is set(<b a c d e f g>) cmp set(<b a c d e f h>), Less,
    'Set cmp compares sorted elements';
is set(<a b>) cmp set(<a b>), Same, 'equal Sets compare Same';
is bag(<a a b>) cmp bag(<a b b>), More, 'Bag cmp compares sorted pairs';
is mix(<a b>) cmp mix(<a>), More, 'Mix with more elements is More';
is-deeply (set(<b c>), set(<a b>), set(<a>)).sort.map(*.keys.sort.join).list,
    ('a', 'ab', 'bc'), 'sort of Sets orders them by content';

# behavior that must not change
is (10, 5) cmp (7, 6), More, 'List cmp List still compares element by element';
is <a b> cmp "a b", Same, 'List cmp Str still compares stringifications';
is (1..3) cmp (1..3), Same, 'Range cmp Range still works';
is (1, 2) cmp any((1, 2), (3, 4)), any(Same, Less), 'Junctions still autothread';

# vim: expandtab shiftwidth=4
