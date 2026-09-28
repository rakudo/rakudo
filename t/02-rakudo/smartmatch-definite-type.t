use Test;

plan 33;

# A declared type is itself an undefined type object, so a smartmatch
# against a :U, :D or coercion type must check the runtime value of a
# variable, parameter, attribute or routine call, not its declared type.

my $untyped = 42;
is-deeply ($untyped ~~ Mu:U), False,
  'a defined value in an untyped variable does not match Mu:U';
is-deeply ($untyped !~~ Mu:U), True,
  'a defined value in an untyped variable does not match Mu:U when negated';

sub untyped-param($x) { $x ~~ Mu:U }
sub untyped-param-negated($x) { $x !~~ Mu:U }
is-deeply untyped-param(42), False,
  'a defined value in an untyped parameter does not match Mu:U';
is-deeply untyped-param-negated(42), True,
  'a defined value in an untyped parameter does not match Mu:U when negated';

my Int $typed = 42;
is-deeply ($typed ~~ Int:U), False,
  'a defined value in an Int variable does not match Int:U';
is-deeply ($typed !~~ Int:U), True,
  'a defined value in an Int variable does not match Int:U when negated';
is-deeply ($typed ~~ (Int:U)), False,
  'a defined value in an Int variable does not match (Int:U)';
is-deeply ($typed !~~ (Int:U)), True,
  'a defined value in an Int variable does not match (Int:U) when negated';
is-deeply (do if $typed ~~ Int:U { 'undefined' } else { 'defined' }), 'defined',
  'a defined value in an Int variable takes the else branch of an Int:U condition';

sub typed-param(Int $x) { $x ~~ Int:U }
is-deeply typed-param(42), False,
  'a defined value in an Int parameter does not match Int:U';

my Int:D $definite = 42;
is-deeply ($definite ~~ Mu:U), False,
  'a value in an Int:D variable does not match Mu:U';
is-deeply ($definite ~~ Int:U), False,
  'a value in an Int:D variable does not match Int:U';

my int $native = 42;
is-deeply ($native ~~ Int:U), False,
  'a value in an int variable does not match Int:U';

sub returns-int(--> Int) { 42 }
is-deeply (returns-int() ~~ Int:U), False,
  'a defined value from a routine returning Int does not match Int:U';
is-deeply (returns-int() ~~ (Int:U)), False,
  'a defined value from a routine returning Int does not match (Int:U)';

my @array = 1, 2;
is-deeply (@array.elems ~~ Mu:U), False,
  'a defined value from a method call does not match Mu:U';
is-deeply (@array !~~ Positional:U), True,
  'an array variable does not match Positional:U when negated';

my &callback = -> { };
is-deeply (&callback ~~ Callable:U), False,
  'a block in a callable variable does not match Callable:U';

my class HasAttribute {
    has Int $.attribute = 42;
    method attribute-undefined() { $!attribute ~~ Int:U }
}
is-deeply HasAttribute.new.attribute-undefined, False,
  'a defined value in an Int attribute does not match Int:U';

my constant CoercedFromUndefinedStr = Metamodel::CoercionHOW.new_type(Int, Str:U);
my subset UndefinedOrLong of Str where { !.defined || .chars > 3 };
my constant CoercedFromLongStr = Metamodel::CoercionHOW.new_type(Int, UndefinedOrLong);
my Str $str = "ab";
is-deeply ($str ~~ CoercedFromUndefinedStr), False,
  'a defined value in a Str variable does not match a coercion from Str:U';
is-deeply ($str !~~ CoercedFromUndefinedStr), True,
  'a defined value in a Str variable does not match a coercion from Str:U when negated';
is-deeply ($str ~~ CoercedFromLongStr), False,
  'a short value in a Str variable does not match a coercion from a subset of long Str';

my $junction = 1 | 2;
is-deeply ($junction ~~ Mu:U), False,
  'a junction of defined values in an untyped variable does not match Mu:U';
is-deeply ($junction !~~ Mu:U), True,
  'a junction of defined values in an untyped variable does not match Mu:U when negated';

my $undefined;
is-deeply ($undefined ~~ Mu:U), True,
  'an undefined untyped variable matches Mu:U';
is-deeply ($undefined !~~ Mu:U), False,
  'an undefined untyped variable matches Mu:U when negated';

my Int $typed-undefined;
is-deeply ($typed-undefined ~~ Int:U), True,
  'an undefined Int variable matches Int:U';
is-deeply ($typed-undefined ~~ Int:D), False,
  'an undefined Int variable does not match Int:D';

my Int:U $undefined-only;
is-deeply ($undefined-only ~~ Int:U), True,
  'an Int:U variable matches Int:U';

is-deeply (with 42 { $_ ~~ Mu:U }), False,
  'a defined topic does not match Mu:U';

sub typed-topic(Int $_) { when Int:U { 'undefined' }; default { 'defined' } }
is-deeply typed-topic(42), 'defined',
  'a defined value in an Int topic parameter does not take an Int:U when';

my $type-object = Mu:U;
is-deeply ($type-object ~~ Mu:U), True,
  'the Mu:U type object in a variable matches Mu:U';
is-deeply ($type-object !~~ Mu:U), False,
  'the Mu:U type object in a variable matches Mu:U when negated';

# vim: expandtab shiftwidth=4
