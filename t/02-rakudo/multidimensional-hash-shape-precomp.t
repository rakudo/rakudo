use lib <t/02-rakudo/test-packages>;
use Test;
use MultidimensionalHashShape;

plan 13;

my $storage = MultidimensionalHashShape.new;
$storage.add('a', 'b', 'c', 42);

is $storage.get('a', 'b', 'c'), 42,
    'a precompiled attribute of three dimensions holds a value under a key for each dimension';
is-deeply $storage.keys.List, (('a', 'b', 'c'),),
    'a precompiled attribute of three dimensions lists a key for each dimension';
throws-like { $storage.add('a', 'b', Str, 1) }, X::TypeCheck::Binding,
    'a precompiled attribute of three dimensions checks the key type of its last dimension';

my %typed := $storage.typed;
is %typed{'a';1}, 42,
    'a hash of two dimensions made in a precompiled method holds its value';
ok %typed.WHAT =:= Hash[Int,(Str,Int)],
    'a hash of two dimensions made in a precompiled method is of the type named here';
throws-like { %typed{'a';2} = 'x' }, X::TypeCheck::Assignment, expected => Int,
    'a hash of two dimensions made in a precompiled method checks its value type';
throws-like { %typed{'a';'b'} = 1 }, X::TypeCheck::Binding, expected => Int,
    'a hash of two dimensions made in a precompiled method checks its key types';
my Int %bound{Str;Int} := %typed;
ok %bound =:= %typed,
    'a hash of two dimensions made in a precompiled method binds to a declaration here';
ok $storage.type-object.WHAT =:= Hash[Int,(Str,Int)],
    'a hash type of two dimensions named in a precompiled method is the one named here';

my $generic = MultidimensionalHashShapeGeneric[Int].new;
$generic.stored{'a';1} = 42;
is $generic.stored{'a';1}, 42,
    'a hash of two dimensions in a precompiled role stores a value of the type the role takes';
throws-like { $generic.stored{'a';2} = 'x' }, X::TypeCheck::Assignment, expected => Int,
    'a hash of two dimensions in a precompiled role checks the value type the role takes';
throws-like { $generic.stored{'a';'b'} = 1 }, X::TypeCheck::Binding, expected => Int,
    'a hash of two dimensions in a precompiled role checks the key type the role takes';
ok $generic.definite{'a';'b'} === Int,
    'a missing value of a definite type in a precompiled role is the base type';

# vim: expandtab shiftwidth=4
