use Test;

plan 12;

# A smartmatch on a routine call runs the call and checks what it returns,
# whatever the routine's return type. A native variable's value is boxed
# before the match, so its native type decides nothing.

my $calls = 0;
sub returns-nil(--> Int) { $calls++; Nil }
is-deeply (returns-nil() ~~ Int), False,
  'a routine returning Nil despite an Int return type does not match Int';
is-deeply (returns-nil() !~~ Int), True,
  'a routine returning Nil despite an Int return type does not match Int when negated';
is $calls, 2,
  'a routine with an Int return type is called for each match';

my class HasMethod {
    method returns-nil(--> Int) { Nil }
}
is-deeply (HasMethod.returns-nil ~~ Int), False,
  'a method returning Nil despite an Int return type does not match Int';

sub returns-constant(--> 42) { $calls++ }
is-deeply (returns-constant() ~~ Int), True,
  'a routine with a constant return matches Int';
is-deeply (returns-constant() ~~ 42), True,
  'a routine with a constant return matches the constant';
is $calls, 4,
  'a routine with a constant return is called for each match';

my int $native-int = 5;
is-deeply ($native-int ~~ int), False,
  'an int variable does not match int';
is-deeply ($native-int !~~ int), True,
  'an int variable does not match int when negated';
is-deeply ($native-int ~~ Int), True,
  'an int variable matches Int';

my num $native-num = 1e0;
is-deeply ($native-num ~~ num), False,
  'a num variable does not match num';

my str $native-str = "a";
is-deeply ($native-str ~~ str), False,
  'a str variable does not match str';

# vim: expandtab shiftwidth=4
