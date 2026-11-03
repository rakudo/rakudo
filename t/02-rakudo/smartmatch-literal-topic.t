use Test;
use experimental :rakuast;

plan 37;

# A smartmatch against a number or string literal gives what the literal's
# ACCEPTS gives, for a topic that has no usable Numeric, one whose Numeric
# or Str is unusual, and one that is not a Str.

my class NoNumeric { }
my class NumericDies { method Numeric(|) { die "no number" } }
my class NumericTypeObject { method Numeric(|) { Int } }
my class NumericAny { method Numeric(|) { Any } }
my class NumericStr { method Numeric(|) { "abc" } }
my class NumericFails {
    has $.failure;
    method Numeric(|) { $!failure = Failure.new("no number") }
}
my class NumericRoleDies does Numeric {
    multi method Numeric(NumericRoleDies:D:) { die "no number" }
}
my class IntNumericDies is Int { method Numeric(|) { die "no number" } }
my class IntNumericDiffers is Int { method Numeric(|) { 99 } }
my class StrNumericDies is Str { method Numeric(|) { die "no number" } }
my class StrDies { method Str(|) { die "no string" } }
my class StrStringyDiffer {
    method Str(|) { "str" }
    method Stringy(|) { "stringy" }
}
my class MuTopic is Mu { }

my $failure = Failure.new("unhandled");
is-deeply ($failure ~~ 42), False,
  'a Failure does not match an Int literal';
ok $failure.handled,
  'a Failure matched against a literal is handled';
is-deeply (Failure.new("unhandled") !~~ 42), True,
  'a Failure does not match an Int literal when negated';
is-deeply (Failure.new("unhandled") ~~ 42e0), False,
  'a Failure does not match a Num literal';

my $no-numeric = NoNumeric.new;
is-deeply ($no-numeric ~~ 42), False,
  'an object without a Numeric does not match an Int literal';
is-deeply ($no-numeric !~~ 42), True,
  'an object without a Numeric does not match an Int literal when negated';
is-deeply ($no-numeric !~~ 42e0), True,
  'an object without a Numeric does not match a Num literal when negated';

my $numeric-dies = NumericDies.new;
is-deeply ($numeric-dies ~~ 42), False,
  'an object whose Numeric dies does not match an Int literal';
is-deeply ($numeric-dies !~~ 42), True,
  'an object whose Numeric dies does not match an Int literal when negated';

my $numeric-fails = NumericFails.new;
is-deeply ($numeric-fails ~~ 42), False,
  'an object whose Numeric returns a Failure does not match an Int literal';
ok $numeric-fails.failure.handled,
  'the Failure a Numeric returned is handled by the match';

my $numeric-str = NumericStr.new;
is-deeply ($numeric-str ~~ 42), False,
  'an object whose Numeric returns a word does not match an Int literal';
is-deeply ($numeric-str !~~ 42), True,
  'an object whose Numeric returns a word does not match an Int literal when negated';

my @warnings;
{
    CONTROL { when CX::Warn { @warnings.push: .message; .resume } }
    my $numeric-type-object = NumericTypeObject.new;
    is-deeply ($numeric-type-object ~~ 42), False,
      'an object whose Numeric returns Int does not match an Int literal';
    is-deeply ($numeric-type-object !~~ 42), True,
      'an object whose Numeric returns Int does not match an Int literal when negated';
    is-deeply (NumericAny.new ~~ 42), False,
      'an object whose Numeric returns Any does not match an Int literal';
}
is-deeply @warnings, [],
  'objects whose Numeric returns a type object match an Int literal without a warning';

is-deeply (NumericRoleDies.new ~~ 42), False,
  'a Numeric object whose Numeric dies does not match an Int literal';
is-deeply (StrNumericDies.new(value => "42") ~~ 42), False,
  'a Str object whose Numeric dies does not match an Int literal';
is-deeply (IntNumericDies.new(42) ~~ 42), True,
  'an Int object whose Numeric dies matches an equal Int literal';
is-deeply (IntNumericDies.new(42) ~~ 42e0), False,
  'an Int object whose Numeric dies does not match a Num literal';
is-deeply (IntNumericDiffers.new(42) ~~ 42), True,
  'an Int object matches an Int literal equal to its value rather than its Numeric';

dies-ok { MuTopic.new ~~ 42 },
  'an object that is not Any dies against an Int literal as ACCEPTS does';

my $word-match = "abc" ~~ / \w+ /;
is-deeply ($word-match ~~ 42), False,
  'a Match of a word does not match an Int literal';

my $number-match = "42" ~~ / \d+ /;
is-deeply ($number-match ~~ 42), True,
  'a Match of a number matches an Int literal';

is-deeply (given Failure.new("unhandled") { when 42 { 'matched' }; default { 'unmatched' } }), 'unmatched',
  'a Failure topic does not take an Int literal when';

my $modifier-result = 'unmatched';
$modifier-result = 'matched' when 42 given Failure.new("unhandled");
is $modifier-result, 'unmatched',
  'a Failure topic does not take an Int literal when statement modifier';

my $int-type-object = Int;
is-deeply ($int-type-object !~~ 42), True,
  'an undefined topic does not match an Int literal when negated';
my $any-type-object = Any;
is-deeply ($any-type-object !~~ "a"), True,
  'an undefined topic does not match a Str literal when negated';

my $str-stringy-differ = StrStringyDiffer.new;
is-deeply ($str-stringy-differ ~~ "str"), True,
  'a topic matches a Str literal equal to its Str';
is-deeply ($str-stringy-differ !~~ "str"), False,
  'a topic matches a Str literal equal to its Str when negated';

my $mixed-in-str = "abc" but "shown";
is-deeply ($mixed-in-str ~~ "abc"), True,
  'a Str topic matches a Str literal equal to its own value';
is-deeply ($mixed-in-str ~~ "shown"), False,
  'a Str topic does not match a Str literal equal to its Str';

dies-ok { StrDies.new ~~ "a" },
  'a topic whose Str dies dies against a Str literal as ACCEPTS does';

sub match-str-literal(Mu $topic is raw, Str:D $literal, Str:D $infix) {
    EVAL RakuAST::ApplyInfix.new(
      left  => RakuAST::Var::Lexical.new('$topic'),
      infix => RakuAST::Infix.new($infix),
      right => RakuAST::StrLiteral.new($literal)
    )
}
is-deeply match-str-literal($str-stringy-differ, "str", '~~'), True,
  'a topic matches a built Str literal equal to its Str';
is-deeply match-str-literal($str-stringy-differ, "stringy", '~~'), False,
  'a topic does not match a built Str literal equal to its Stringy';
is-deeply match-str-literal($mixed-in-str, "abc", '!~~'), False,
  'a Str topic matches a built Str literal equal to its own value when negated';

# vim: expandtab shiftwidth=4
