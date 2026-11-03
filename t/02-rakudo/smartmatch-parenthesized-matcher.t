use Test;

plan 6;

# A smartmatch against a type in parentheses whose result is known at
# compile time must still compile and give that result.

is-deeply (42 ~~ (Int)), True,
  'a literal Int matches (Int)';
is-deeply (42 !~~ (Int)), False,
  'a literal Int matches (Int) when negated';
is-deeply ("x" ~~ (Int)), False,
  'a literal Str does not match (Int)';

my Int $typed = 42;
is-deeply ($typed ~~ (Int)), True,
  'an Int variable matches (Int)';
is-deeply ($typed !~~ (Int)), False,
  'an Int variable matches (Int) when negated';
is-deeply ($typed ~~ (Cool)), True,
  'an Int variable matches (Cool)';

# vim: expandtab shiftwidth=4
