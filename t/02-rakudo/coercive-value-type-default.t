use Test;

plan 19;

ok Hash[Int()].new<a> === Int,
    'a missing key of a hash of a coercive type gives the target type';
ok Hash[Int(),Str].new<a> === Int,
    'a missing key of an object hash of a coercive type gives the target type';
ok Hash[Int(),Str].new<a>:delete === Int,
    'deleting a missing key of an object hash of a coercive type gives the target type';
ok Array[Int()].new[0] === Int,
    'a missing element of an array of a coercive type gives the target type';
{
    my Hash[Int()] $h;
    ok $h<a> === Int,
        'a key of a type object of a hash of a coercive type gives the target type';
    my Array[Int()] $a;
    ok $a[0] === Int,
        'an element of a type object of an array of a coercive type gives the target type';
}

is Hash[Int()].^name, 'Hash[Int(Any)]',
    'the name of a hash of a coercive type does not name its default';
is Hash[Int(),Str].^name, 'Hash[Int(Any),Str]',
    'the name of an object hash of a coercive type does not name its default';
is Array[Int()].^name, 'Array[Int(Any)]',
    'the name of an array of a coercive type does not name its default';
ok Hash.^parameterize(Int(), Str, Int) =:= Hash[Int(),Str],
    'naming the target of a coercive type as the default gives the same object hash type';
is Hash.^parameterize(Int(), Str, Int).^name, 'Hash[Int(Any),Str]',
    'naming the target of a coercive type as the default keeps the object hash name';
ok Hash.^parameterize(Int(), Str(Any), Int) =:= Hash[Int()],
    'naming the target of a coercive type as the default gives the same hash type';
is Hash.^parameterize(Int(), Str(Any), Int).^name, 'Hash[Int(Any)]',
    'naming the target of a coercive type as the default keeps the hash name';
is Hash.^parameterize(Int(), Str, Any).^name, 'Hash[Int(Any),Str,Any]',
    'naming another default than the target of a coercive type names it';

ok Hash[Hash[Int]()].new<a> === Hash[Int],
    'a missing key of a hash of a coercive hash type gives the target type';

{
    my $h = Hash[Int()].new;
    $h<a> = '42';
    isa-ok $h<a>, Int,
        'a hash of a coercive type still coerces what is assigned';
}

{
    my &primed = sub (Str $a, Array[Int] $b, Array[Int()] $c, Hash[Int()] $d) { }.assuming('a');
    my @types := &primed.signature.params.map(*.type).List;
    ok @types[0] =:= Array[Int],
        'an array type is the same type when .assuming names it again';
    ok @types[1] =:= Array[Int()],
        'an array of a coercive type is the same type when .assuming names it again';
    ok @types[2] =:= Hash[Int()],
        'a hash of a coercive type is the same type when .assuming names it again';
}

# vim: expandtab shiftwidth=4
