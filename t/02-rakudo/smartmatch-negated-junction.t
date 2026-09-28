use Test;

plan 5;

# A negated smartmatch whose matcher is a type in parentheses must stay
# negated when the topic turns out to be a Junction at runtime.

my $all-int = 1 | 2;
my $some-int = 1 | "a";
my $type-objects = Int | Str;

is-deeply ($all-int !~~ (Int)), False,
  'a junction of Int values matches (Int) when negated';
is-deeply ($some-int !~~ (Int)), False,
  'a junction with one Int value matches (Int) when negated';
is-deeply ($some-int !~~ (Str)), False,
  'a junction with one Str value matches (Str) when negated';
is-deeply ($type-objects !~~ (Int:U)), False,
  'a junction of type objects matches (Int:U) when negated';
is-deeply ($all-int !~~ (Int:U)), True,
  'a junction of defined Int values does not match (Int:U) when negated';

# vim: expandtab shiftwidth=4
