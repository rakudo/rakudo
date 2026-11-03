use Test;

plan 12;

# https://github.com/rakudo/rakudo/issues/4361
# array[T] with a still-generic T defers its native-type check until the
# surrounding role is instantiated, like Blob[T] already did.

# A generic role with an array[T] attribute compiles at all.
my role HasArray[::T] {
    has array[T] $.a;
}
pass 'role with array[T] attribute compiles';

# Instantiating with a native int type produces an array[int] attribute.
is HasArray[int].new.WHAT.^attributes.head.type.^name, 'array[int]',
  'attribute type instantiates to array[int]';

# The instantiated attribute accepts a matching typed array.
my $with-int = HasArray[int].new(a => array[int].new(1, 2, 3));
is $with-int.a.^name, 'array[int]',
  'attribute holds an array[int] after instantiation with int';

# The attribute behaves as a native int array.
is $with-int.a.sum, 6, 'attribute contents survive as native ints';

# A different native type instantiates independently.
is HasArray[num].new(a => array[num].new(2e0)).a.^name, 'array[num]',
  'attribute type instantiates to array[num] for a num role instance';

# A native str type takes the string parameterization path.
is HasArray[str].new(a => array[str].new(<a b>)).a.^name, 'array[str]',
  'attribute type instantiates to array[str] for a str role instance';

# Sized native types resolve through their own parameterization paths.
is HasArray[uint8].new.WHAT.^attributes.head.type.^name, 'array[uint8]',
  'attribute type instantiates to array[uint8] for a sized native type';

# A default expression can parameterize with the resolved type at build time.
my role DefaultArray[::T] {
    has array[T] $.a = array[T].new;
}
my $defaulted = DefaultArray[int].new;
$defaulted.a.push(42);
is $defaulted.a.^name, 'array[int]',
  'attribute default array[T].new produces an array[int]';
is $defaulted.a[0], 42, 'defaulted attribute stores native ints';

# The deferred parameterization reports a name based on the generic.
is array.^parameterize(Metamodel::GenericHOW.new_type(:name<T>)).^name,
  'array[T]',
  'a generic parameterization names itself array[T]';

# Instantiating with a non-native type still dies, now at instantiation time.
throws-like { my role BadArray[::T] { has array[T] $.a }; BadArray[Str].new },
  Exception,
  message => /'Can only parameterize array with a native type'/,
  'instantiating with a non-native type dies with the native-type error';

# A concrete non-native parameterization still dies immediately.
throws-like { array.^parameterize(Int) },
  Exception,
  message => /'Can only parameterize array with a native type'/,
  'directly parameterizing with a non-native type still dies';

# vim: expandtab shiftwidth=4
