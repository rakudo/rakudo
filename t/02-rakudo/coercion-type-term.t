use Test;

plan 23;

# A type name followed by a type in parentheses is a coercion type, also
# when that type has a smiley or type arguments. A leading :: on it looks
# the type up and declares nothing.

is Int(Str:D).^name, 'Int(Str:D)',
  'a coercion from a :D type is a coercion type';
is Int(Str:U).^name, 'Int(Str:U)',
  'a coercion from a :U type is a coercion type';
is Int:D(Str:D).^name, 'Int:D(Str:D)',
  'a :D coercion from a :D type is a coercion type';
is Int(Str:_).^name, 'Int(Str)',
  'a coercion from a :_ type is a coercion type';
is Int(Array[Int]).^name, 'Int(Array[Int])',
  'a coercion from a parameterized type is a coercion type';

my class Local { }
is Int(Local:D).^name, 'Int(Local:D)',
  'a coercion from a lexical class with a smiley is a coercion type';

is-deeply ("x" ~~ Int(Str:U)), False,
  'a defined Str does not match a coercion from Str:U';
is-deeply (Str ~~ Int(Str:U)), True,
  'the Str type object matches a coercion from Str:U';

is Int(::Str).^name, 'Int(Str)',
  'a leading :: in a coercion looks the type up';
is-deeply (do { my $coercion = Int(::Str); Str.^name }), 'Str',
  'a leading :: in a coercion declares no type capture';

my class HasClassCoercion {
    method coercion-name() { Int(::?CLASS).^name }
}
is HasClassCoercion.coercion-name, 'Int(HasClassCoercion)',
  'a coercion from ::?CLASS is a coercion type';

is Int(Rat(::Str)).^name, 'Int(Rat(Str))',
  'a leading :: in a nested coercion looks the type up';
is-deeply (do { my $coercion = Int(Rat(::Str)); Str.^name }), 'Str',
  'a leading :: in a nested coercion declares no type capture';

my class Outer::Inner { }
is Int(::Outer::Inner).^name, 'Int(Outer::Inner)',
  'a leading :: in a coercion looks a nested package name up';

my class HasDefiniteClassCoercion {
    method coercion-name() { Int(::?CLASS:D).^name }
}
ok HasDefiniteClassCoercion.coercion-name.starts-with('Int(HasDefiniteClassCoercion'),
  'a coercion from ::?CLASS with a smiley is a coercion type';

my role HasGenericCoercion[::T] {
    method coercion-name() { Int(T:D).^name }
}
my class ConsumesGenericCoercion does HasGenericCoercion[Str] { }
ok ConsumesGenericCoercion.coercion-name.starts-with('Int('),
  'a coercion from a type parameter with a smiley is a coercion type';

my subset Positive of Int where * > 0;
is Int(Positive:D).^name, 'Int(Positive:D)',
  'a coercion from a subset with a smiley is a coercion type';

is Int(Str(Numeric)).^name, 'Int(Str(Numeric))',
  'a coercion from a coercion type is a coercion type';

is Array[Int(Str:D)].^name, 'Array[Int(Str:D)]',
  'a coercion from a :D type is a coercion type as a type argument';

my constant NotAType = "42";
is-deeply Int(NotAType), 42,
  'a constant that is not a type in parentheses is a call';

sub not-a-type { "7" }
is-deeply Int(not-a-type), 7,
  'a sub call in parentheses is a call';

is Int().^name, 'Int(Any)',
  'empty parentheses are a coercion from Any';

sub coerced(Int(Str:D) $value) { $value }
is-deeply coerced("5"), 5,
  'a coercion type with a :D constraint in a signature coerces';

# vim: expandtab shiftwidth=4
