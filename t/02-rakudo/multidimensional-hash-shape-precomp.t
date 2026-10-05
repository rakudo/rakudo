use lib <t/02-rakudo/test-packages>;
use Test;
use MultidimensionalHashShape;

plan 12;

my $storage = MultidimensionalHashShape.new;
$storage.add('a', 'b', 'c', 42);

is-deeply $storage.find('a', 'b'), (42,).Seq,
    'a precompiled attribute of three dimensions finds a nested hash by a shorter key path';
is-deeply $storage.find('a', 'x'), Empty,
    'a precompiled attribute of three dimensions has no missing key path';
ok $storage.level('a').keyof === Str:D,
    'a nested hash of a precompiled attribute is keyed by the type of its dimension';
throws-like { $storage.add('a', 'b', 42, 1) }, X::TypeCheck::Binding::Parameter,
    message => /"parameter 'key'"/,
    'a precompiled attribute of three dimensions checks the key type of its last dimension';

my %typed := $storage.typed;
is %typed<a>{1}, 42,
    'a hash of two dimensions made in a precompiled method holds its value';
ok %typed.WHAT =:= (my Int %{Str;Int}).WHAT,
    'a hash of two dimensions made in a precompiled method is of the type declared here';
throws-like { %typed<a>{2} = 'x' }, X::TypeCheck::Assignment, expected => Int,
    'a hash of two dimensions made in a precompiled method checks its value type';
%typed<b> = :{ 2 => 43 };
is %typed<b>{2}, 43,
    'a hash of two dimensions made in a precompiled method coerces a nested hash';
throws-like { %typed<c> = { 2 => 44 } }, X::TypeCheck::Binding::Parameter,
    'a hash of two dimensions made in a precompiled method checks the keys of a nested hash';

my $generic = MultidimensionalHashShapeGeneric[Int].new;
$generic.stored<a><b> = 42;
is $generic.stored<a><b>, 42,
    'a hash of two dimensions in a precompiled role stores a value of the type the role takes';
throws-like { $generic.stored<a><c> = 'x' }, X::TypeCheck::Assignment, expected => Int,
    'a hash of two dimensions in a precompiled role checks the value type the role takes';
ok $generic.definite<a><b> === Int,
    'a missing value of a definite type in a precompiled role is the base type';

# vim: expandtab shiftwidth=4
