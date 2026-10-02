use lib <t/02-rakudo/test-packages>;
use Test;
use GenericCoercionParam;

plan 11;

# A parameter typed with a coercion to a role's type capture needs the
# full binder. A precompiled role body must not read such a parameter,
# or any other in the same routine, from a frame-local the binder never
# writes.

class IntCoercion does GenericCoercionParam[Int] { }

is-deeply (try IntCoercion.new(value => "1").value), 1,
    'a named T() parameter of a BUILD submethod coerces';
is-deeply (try IntCoercion.positional("2")), 2,
    'a positional T() parameter coerces';
is-deeply (try IntCoercion.named(v => "3")), 3,
    'a named T() parameter of a method coerces';
is-deeply (try IntCoercion.optional), 7,
    'the default of an optional T() parameter coerces';
is-deeply (try IntCoercion.definite("4")), 4,
    'a T:D() parameter coerces';
is-deeply (try IntCoercion.constrained("5")), 5,
    'a T(Str) parameter coerces';
is-deeply (try IntCoercion.parameterized([1, 2]).WHAT), Array[Int],
    'an Array[T]() parameter coerces';
is-deeply (try IntCoercion.with-sibling("a", "6")), ("a", 6, "IntCoercion"),
    'a sibling parameter and self are bound next to a T() parameter';
is-deeply (try IntCoercion.multi("8")), 8,
    'a T() parameter of a multi method coerces';
is-deeply (try IntCoercion.in-sub("9")), 9,
    'a T() parameter of a sub inside a role method coerces';
is-deeply (try IntCoercion.in-pointy("10")), 10,
    'a T() parameter of a pointy block inside a role method coerces';
