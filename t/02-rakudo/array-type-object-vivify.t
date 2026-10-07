use Test;

plan 40;

my class VivifyArraySubclass is Array { }
my class VivifyArrayNoNew is Array { method new(|) { die 'no new' } }
my subset VivifyArraySubset of Array;
my subset VivifyTypedArraySubset of Array[Int];

{
    my Array[Int] $a;
    $a[1] = 42;
    isa-ok $a, Array[Int],
        'assigning to an element of an Array[Int] type object vivifies an Array[Int]';
    is $a[1], 42,
        'the vivified Array[Int] holds the assigned value';
    throws-like { $a[1] = 'x' }, X::TypeCheck::Assignment, expected => Int,
        'the element an Array[Int] type object vivifies checks its value type';
}

{
    my Array[Int] $a;
    throws-like { $a[0] = 'x' }, X::TypeCheck::Assignment, expected => Int,
        'vivifying an Array[Int] type object checks the value type';
    nok $a.defined,
        'an Array[Int] type object stays undefined when the value is refused';
}

{
    my Array[Int] $a;
    ok $a[0] === Int,
        'reading an element of an Array[Int] type object gives its value type';
    nok $a.defined,
        'reading an element of an Array[Int] type object leaves it undefined';
}

{
    my Array $a;
    $a[0] = 42;
    is-deeply $a, [42],
        'assigning to an element of an Array type object vivifies an Array';
}

{
    my VivifyArraySubclass $a;
    $a[0] = 42;
    isa-ok $a, VivifyArraySubclass,
        'assigning to an element of an Array subclass type object vivifies the subclass';
}

{
    my $i = -1;
    my Array[Int] $a;
    throws-like { $a[$i] = 42 }, X::OutOfRange,
        'assigning to a negative index of an Array[Int] type object throws';
    nok $a.defined,
        'an Array[Int] type object stays undefined when the index is refused';
    my $b := Array[Int].new;
    my $c;
    my Array[Int] $d;
    $c := $d[$i];
    $d = $b;
    throws-like { $c = 42 }, X::OutOfRange,
        'assigning to a negative index of an Array[Int] stored after the element was taken throws';
}

{
    my $i = -1;
    my Array[Int] $a;
    my $element := $a[$i];
    throws-like { $element = 42 }, X::OutOfRange,
        'assigning to an element taken at a negative index of an Array[Int] type object throws';
    nok $a.defined,
        'an Array[Int] type object stays undefined when the index of a taken element is refused';
}

{
    my VivifyArrayNoNew $a;
    ok $a[0] === Any,
        'reading an element of an Array subclass type object does not instantiate it';
    nok $a.defined,
        'reading an element of an Array subclass type object leaves it undefined';
}

{
    my VivifyArraySubset $s;
    $s[0] = 42;
    isa-ok $s, Array,
        'assigning to an element of a subset of Array vivifies an Array';
    my VivifyTypedArraySubset $t;
    $t[0] = 42;
    isa-ok $t, Array[Int],
        'assigning to an element of a subset of Array[Int] vivifies an Array[Int]';
    my $d = Array:D;
    $d[0] = 42;
    isa-ok $d, Array,
        'assigning to an element of a definite Array type vivifies an Array';
    my $c = Array[Int]();
    $c[0] = 42;
    isa-ok $c, Array[Int],
        'assigning to an element of a coercion to Array[Int] vivifies an Array[Int]';
}

{
    my Array[Int] $a;
    ($a[0], $a[1]) = 1, 2;
    is-deeply $a.List, (1, 2),
        'two elements taken from an Array[Int] type object before either is assigned both land in it';
}

{
    my class TimesTen is Array {
        multi method ASSIGN-POS(TimesTen:D: Int:D \pos, Mu \value) is raw {
            self.Array::ASSIGN-POS(pos, value * 10)
        }
    }
    my TimesTen $a;
    $a[0] = 1;
    is-deeply $a.List, (10,),
        'assigning to an element of an Array subclass type object goes through its ASSIGN-POS';
}

{
    my Array $a;
    my $element := $a[0];
    $element = Failure.new('stored');
    ok $a[0] ~~ Failure,
        'a Failure assigned to an element an Array type object vivifies is stored';
    $a[0].so;
}

{
    my Array $x;
    my $element := $x[0];
    my Str @typed;
    $x = @typed;
    dies-ok { $element = 42 },
        'an element taken from a type object checks the value type of the array stored in its place';
    my @target;
    my Array $y;
    my $taken := $y[0];
    $y = @target;
    $taken = 42;
    is @target[0], 42,
        'an element taken from a type object binds into the array stored in its place';
}

{
    my class StrictPositions is Array {
        multi method AT-POS(StrictPositions:D: Int:D $pos) is raw {
            self.EXISTS-POS($pos) ?? self.Array::AT-POS($pos) !! Failure.new('no such position')
        }
    }
    my StrictPositions $a;
    throws-like { $a[0][0] = 1 }, Exception, message => 'no such position',
        'a Failure from the AT-POS of an Array subclass type object is thrown';
    my class PassThrough is Array {
        multi method ASSIGN-POS(PassThrough:D: Int:D \pos, Mu \value) { callsame }
    }
    my PassThrough $p;
    my $stored = ($p[0] = Failure.new('stored'));
    ok $p.defined,
        'a Failure an Array subclass type object stores is not taken for a refusal';
    $stored.so;
    $p[0].so;
}

{
    my @bare := Array[Int];
    throws-like { @bare[0] = 1 }, X::Assignment::RO,
        'assigning to an element of an Array[Int] type object outside a container refuses to modify it';
    my class RefusingBind is Array {
        multi method BIND-POS(RefusingBind:D: Int:D \pos, Mu \value) is raw {
            pos == 5 ?? Failure.new('refused') !! callsame
        }
    }
    my RefusingBind $p;
    my $first := $p[0];
    my $sixth := $p[5];
    $first = 1;
    throws-like { $sixth = 2 }, Exception, message => 'refused',
        'an element taken from an Array subclass type object throws when its BIND-POS refuses the index';
}

{
    sub first-two(Array $a) { $a[0], $a[1] }
    my \items = first-two(Array);
    ok items[0] === Array,
        'the first element of an Array type object outside a container is the type object, as for any item';
    ok items[1] ~~ Failure,
        'a further element of an Array type object outside a container is out of range, as for any item';
    items[1].so;
    my Array $s;
    my $element := $s[0];
    $s = Array.new(:shape(2));
    $element = Failure.new('stored');
    ok $s[0] ~~ Failure,
        'a Failure assigned to an element taken from a type object is stored in a shaped array stored in its place';
    $s[0].so;
}

{
    my $i = -1;
    my $untyped;
    throws-like { $untyped[$i] = 1 }, X::OutOfRange,
        'assigning to a negative index of an undefined variable throws';
    nok $untyped.defined,
        'an undefined variable stays undefined when the index is refused';
}

{
    my class OwnListCandidate is Array {
        multi method AT-POS(List:U: Int:D $pos) { "own $pos" }
    }
    is OwnListCandidate.AT-POS(1), 'own 1',
        'a List:U candidate of an Array subclass is called for its type object';
    my class OwnArrayCandidate is Array {
        multi method AT-POS(Array:U: Int:D $pos) { "own $pos" }
    }
    is OwnArrayCandidate.AT-POS(1), 'own 1',
        'an Array:U candidate of an Array subclass is called for its type object';
}

{
    my Array[Int(Str)] $a;
    ($a[0], $a[1]) = 'abc', '1';
    ok $a[0] ~~ Failure,
        'a value that fails to coerce into an element an Array[Int(Str)] type object vivifies is stored as a Failure';
    is $a[1], 1,
        'the next element taken from the same Array[Int(Str)] type object is stored with it';
    $a[0].so;
}

{
    my Array[Int] %h;
    %h<a>[0] = 42;
    isa-ok %h<a>, Array[Int],
        'a typed array value of a hash vivifies as that type';
    throws-like { %h<a>[1] = 'x' }, X::TypeCheck::Assignment, expected => Int,
        'the typed array value checks its value type';
}

# vim: expandtab shiftwidth=4
