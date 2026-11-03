use Test;

# Str.split counts how many of :v, :k, :kv and :p were passed, and uses the
# count both to reject a combination and to pick the mapping it applies.

plan 17;

is-deeply "a1b".split(/\d/, :k).List, ("a", 0, "b"),
  'the :k adverb reports the index of each separator';
throws-like { "abc".split("b", :v, :k) }, X::Adverb,
  'combining two split adverbs is rejected';
is-deeply "abc".split("b", :v, :k(False)).List, ("a","b","c"),
  'an adverb that boolifies false does not count towards the limit';
is-deeply "a1b2c".split(<1 2>, :v).List, ("a","1","b","2","c"),
  'splitting on a list of needles keeps the separators with :v';
is-deeply "a,b,c".split(",", 2).List, ("a", "b,c"),
  'a split limit stops after the requested number of pieces';
is-deeply "a,,b".split(",", :skip-empty).List, ("a","b"),
  'skip-empty drops the empty piece between adjacent separators';

# The adverbs are untyped, so an argument may boolify to anything at all and
# the count is what has to cope with it.
my class Zero  { method Bool { "0" } }
my class Word  { method Bool { "yes" } }
my class Nilly { method Bool { Nil } }
is-deeply "a,b,c".split(",", :v(Zero.new)).List, ("a","b","c"),
  'an adverb whose Bool is a string that numifies to zero does not count';
throws-like { "a,b,c".split(",", :v(Word.new)) }, X::Str::Numeric,
  'an adverb whose Bool is a non-numeric string reports the string';
quietly is-deeply "a,b,c".split(",", :v(Nilly.new)).List, ("a","b","c"),
  'an adverb whose Bool is Nil does not count';
is-deeply "a,b,c".split(",").List, ("a","b","c"),
  'splitting with no adverb at all keeps every piece';

# An adverb may be given a type object, whose Bool a class is free to
# define, so being undefined is not the same as being absent.
my class TruthyType { method Bool { True } }
is-deeply "a,b".split(",", :v(TruthyType)).List, ("a", ",", "b"),
  'a type object whose Bool is True counts as a :v adverb';
throws-like { "a,b".split(",", :v(TruthyType), :k(TruthyType)) }, X::Adverb,
  'two type objects whose Bool is True are still rejected as a combination';
is-deeply "a,b".split(",", :v(Any)).List, ("a","b"),
  'an adverb given Any counts no more than one left out';

# The count is reached from every split, and the adverb free path is
# the one every ordinary call takes.
is-deeply "a1b2c".split(/\d/).List, ("a","b","c"),
  'splitting on a regex with no adverb keeps every piece';
is-deeply "a1b2c".split(<1 2>).List, ("a","b","c"),
  'splitting on a list of needles with no adverb keeps every piece';

# Each of the four adverbs is counted separately, and the count picks the
# mapping as well as rejecting a combination.
is-deeply "a,b".split(",", :kv).List, ("a", 0, ",", "b"),
  'the :kv adverb reports the index and the separator';
is-deeply "a,b".split(",", :p).List, ("a", 0 => ",", "b"),
  'the :p adverb pairs the index with the separator';

# vim: expandtab shiftwidth=4
