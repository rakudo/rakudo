use Test;

plan 83;

my class VivifyHashSubclass is Hash { }
my class VivifyHashNoNew is Hash { method new(|) { die 'no new' } }
my subset VivifyHashSubset of Hash;
my subset VivifyTypedHashSubset of Hash[Int];
my subset VivifyObjectHashSubset of Hash[Int,Str];

{
    my Hash[Int] $h;
    $h<a> = 42;
    isa-ok $h, Hash[Int],
        'assigning to a key of a Hash[Int] type object vivifies a Hash[Int]';
    is $h<a>, 42,
        'the vivified Hash[Int] holds the assigned value';
    throws-like { $h<a> = 'x' }, X::TypeCheck::Assignment, expected => Int,
        'the element a Hash[Int] type object vivifies checks its value type';
}

{
    my Hash[Int] $h;
    throws-like { $h<a> = 'x' }, X::TypeCheck::Assignment, expected => Int,
        'vivifying a Hash[Int] type object checks the value type';
    nok $h.defined,
        'a Hash[Int] type object stays undefined when the value is refused';
}

{
    my Hash[Int,Str] $h;
    $h<a> = 42;
    isa-ok $h, Hash[Int,Str],
        'assigning to a key of an object hash type object vivifies it';
    is-deeply $h.keys, ('a',).Seq,
        'the vivified object hash holds the assigned key';
    throws-like { $h<a> = 'x' }, X::TypeCheck::Assignment, expected => Int,
        'the element an object hash type object vivifies checks its value type';
}

{
    my Hash[Int,Str] $h;
    throws-like { $h{42} = 1 }, X::TypeCheck::Binding::Parameter,
        'vivifying an object hash type object checks the key type';
    nok $h.defined,
        'an object hash type object stays undefined when the key is refused';
}

{
    my Hash[Int,Str] $h;
    ok $h<a> === Int,
        'reading a key of an object hash type object gives its value type';
    nok $h<a>:exists,
        'a key of an object hash type object does not exist';
    is-deeply $h<a>:delete, Nil,
        'deleting a key of an object hash type object gives Nil';
    nok $h.defined,
        'reading, testing and deleting keys leave the type object undefined';
}

{
    my Hash[Int] $h;
    ok $h<a> === Int,
        'reading a key of a Hash[Int] type object gives its value type';
}

{
    my Hash[Int,Str] $h;
    my $value = 42;
    $h<a> := $value;
    isa-ok $h, Hash[Int,Str],
        'binding a key of an object hash type object vivifies it';
    $value = 43;
    is $h<a>, 43,
        'binding a key of an object hash type object binds the container';
    my Hash[Int,Str] $g;
    throws-like { $g<a> := 'x' }, X::TypeCheck::Binding::Parameter,
        'binding a key of an object hash type object checks the value type';
}

{
    my Hash[Int] $h;
    my $value = 42;
    $h<a> := $value;
    isa-ok $h, Hash[Int],
        'binding a key of a Hash[Int] type object vivifies it';
    $value = 43;
    is $h<a>, 43,
        'binding a key of a Hash[Int] type object binds the container';
    my Hash[Int] $g;
    throws-like { $g<a> := 'x' }, X::TypeCheck::Binding::Parameter,
        'binding a key of a Hash[Int] type object checks the value type';
}

{
    my Hash $h;
    my $value = 42;
    $h<a> := $value;
    isa-ok $h, Hash,
        'binding a key of a Hash type object vivifies it';
    $value = 43;
    is $h<a>, 43,
        'binding a key of a Hash type object binds the container';
}

{
    my VivifyHashSubclass $h;
    $h<a> = 42;
    isa-ok $h, VivifyHashSubclass,
        'assigning to a key of a Hash subclass type object vivifies the subclass';
    my VivifyHashSubclass $g;
    $g<a> := 42;
    isa-ok $g, VivifyHashSubclass,
        'binding a key of a Hash subclass type object vivifies the subclass';
}

{
    my Hash[Int(Str)] $h;
    $h<a> = '42';
    is-deeply $h<a>, 42,
        'vivifying a Hash[Int(Str)] type object coerces the value';
}

{
    my Hash[Int,Str] $h;
    is-deeply ($h.keys, $h.values, $h.kv, $h.antipairs).map(*.elems), (0, 0, 0, 0),
        'an object hash type object has no keys, values, kv or antipairs';
    ok $h.list.head =:= Hash[Int,Str],
        'an object hash type object iterates as itself, as a Hash type object does';
}

{
    my VivifyHashNoNew $h;
    ok $h<a> === Any,
        'reading a key of a Hash subclass type object does not instantiate it';
    nok $h<a>:exists,
        'testing a key of a Hash subclass type object does not instantiate it';
}

{
    my Hash::Ordered $h;
    is-deeply ($h.keys, $h.values, $h.pairs, $h.kv, $h.antipairs).map(*.elems), (0, 0, 0, 0, 0),
        'a Hash::Ordered type object has no keys, values, pairs, kv or antipairs';
    ok $h<a> === Any,
        'reading a key of a Hash::Ordered type object gives Any';
    nok $h.defined,
        'reading a key of a Hash::Ordered type object leaves it undefined';
    $h<b> = 1;
    $h<a> = 2;
    isa-ok $h, Hash::Ordered,
        'assigning to a key of a Hash::Ordered type object vivifies it';
    is-deeply $h.keys.List, <b a>,
        'the vivified Hash::Ordered keeps the order of its keys';
    my Hash::Ordered $g;
    $g<a> := 42;
    isa-ok $g, Hash::Ordered,
        'binding a key of a Hash::Ordered type object vivifies it';
}

{
    my %h is Hash::Ordered;
    my $taken := %h<c>;
    %h<a> = 1;
    $taken = 2;
    %h<b> := 3;
    is-deeply %h.keys.List, <c a b>,
        'a Hash::Ordered keeps keys in the order they are first taken';
    is-deeply %h.values.List, (2, 1, 3),
        'a Hash::Ordered lists values in key order';
    is-deeply %h.pairs.map(*.key).List, <c a b>,
        'a Hash::Ordered lists pairs in key order';
    my %r is Hash::Ordered;
    %r<x>;
    nok %r<x>:exists,
        'reading a missing key of a Hash::Ordered does not store it';
}

{
    my Hash $x;
    my $element := $x<a>;
    my Str %typed;
    $x = %typed;
    throws-like { $element = 42 }, X::TypeCheck::Binding::Parameter,
        'an element taken from a type object checks the value type of the hash stored in its place';
    my %other;
    my %bound;
    %bound<a> := %other<b>;
    my Hash $y;
    my $taken := $y<a>;
    $y = %bound;
    $taken = 42;
    is %bound<a>, 42,
        'an element taken from a type object binds into the hash stored in its place';
}

{
    my class StrictKeys is Hash {
        multi method AT-KEY(StrictKeys:D: Str:D $key) is raw {
            self.EXISTS-KEY($key) ?? self.Hash::AT-KEY($key) !! Failure.new('no such key')
        }
    }
    my StrictKeys $h;
    throws-like { $h<a><b> = 1 }, Exception, message => 'no such key',
        'a Failure from the AT-KEY of a Hash subclass type object is thrown';
    my class PassThrough is Hash {
        multi method ASSIGN-KEY(PassThrough:D: Str:D \key, Mu \value) { callsame }
    }
    my PassThrough $p;
    my $stored = ($p<a> = Failure.new('stored'));
    ok $p.defined,
        'a Failure a Hash subclass type object stores is not taken for a refusal';
    $stored.so;
    $p<a>.so;
}

{
    my Hash[Int] $h;
    ($h<a>, $h<b>) = 1, 2;
    is-deeply $h.keys.sort.List, <a b>,
        'two elements taken from a Hash[Int] type object before either is assigned both land in it';
    my Hash[Int,Str] $o;
    ($o<a>, $o<b>) = 1, 2;
    is-deeply $o.keys.sort.List, <a b>,
        'two elements taken from an object hash type object before either is assigned both land in it';
}

{
    my class LowerCaseKeys is Hash {
        multi method ASSIGN-KEY(LowerCaseKeys:D: Str:D \key, Mu \value) is raw {
            self.Hash::ASSIGN-KEY(key.lc, value)
        }
    }
    my LowerCaseKeys $h;
    $h<A> = 1;
    is-deeply $h.keys.List, ('a',),
        'assigning to a key of a Hash subclass type object goes through its ASSIGN-KEY';
}

{
    my VivifyHashSubset $s;
    $s<a> = 42;
    isa-ok $s, Hash,
        'assigning to a key of a subset of Hash vivifies a Hash';
    my VivifyHashSubset $b;
    $b<a> := 42;
    isa-ok $b, Hash,
        'binding a key of a subset of Hash vivifies a Hash';
    my VivifyTypedHashSubset $t;
    $t<a> = 42;
    isa-ok $t, Hash[Int],
        'assigning to a key of a subset of Hash[Int] vivifies a Hash[Int]';
    my VivifyTypedHashSubset $tb;
    $tb<a> := 42;
    isa-ok $tb, Hash[Int],
        'binding a key of a subset of Hash[Int] vivifies a Hash[Int]';
    my VivifyObjectHashSubset $o;
    $o<a> := 42;
    isa-ok $o, Hash[Int,Str],
        'binding a key of a subset of an object hash vivifies the object hash';
    my VivifyObjectHashSubset $or;
    ok $or<a> === Int,
        'reading a key of a subset of an object hash gives its value type';
    $or<a> = 42;
    isa-ok $or, Hash[Int,Str],
        'assigning to a key of a subset of an object hash vivifies the object hash';
    my VivifyTypedHashSubset %nested;
    %nested<x><a> = 42;
    isa-ok %nested<x>, Hash[Int],
        'a value of a hash typed by a subset of Hash[Int] vivifies a Hash[Int]';
    my $d = Hash:D;
    $d<a> = 42;
    isa-ok $d, Hash,
        'assigning to a key of a definite Hash type vivifies a Hash';
    my $c = Hash[Int]();
    $c<a> = 42;
    isa-ok $c, Hash[Int],
        'assigning to a key of a coercion to Hash[Int] vivifies a Hash[Int]';
}

{
    my Hash[Int,Mu] $j;
    $j{1|2} := 42;
    ok $j.keys.head ~~ Junction,
        'binding a junction key of an object hash type object keyed by Mu stores that key';
    my Hash[Int,Mu] $m;
    $m{Mu} = 42;
    ok $m.keys.head === Mu,
        'assigning to a Mu key of an object hash type object keyed by Mu stores that key';
}

{
    my Hash $h;
    ok $h{1|2} ~~ Junction,
        'a junction key of a Hash type object gives a junction of elements';
    my Hash[Int] $t;
    ok $t{1|2} ~~ Junction,
        'a junction key of a Hash[Int] type object gives a junction of elements';
}

{
    my Hash $h;
    my $key = 'a';
    my $element := $h{$key};
    $key = 'b';
    $element = 1;
    is-deeply $h.keys.List, ('a',),
        'an element taken from a Hash type object keeps the key it was taken with';
    my Hash[Int,Str] $o;
    my $object-key = 'a';
    my $object-element := $o{$object-key};
    $object-key = 'b';
    $object-element = 1;
    is-deeply $o.keys.List, ('a',),
        'an element taken from an object hash type object keeps the key it was taken with';
}

{
    my $untyped;
    my $key = 'a';
    my $element := $untyped{$key};
    $key = 'b';
    $element = 1;
    is-deeply $untyped.keys.List, ('a',),
        'an element taken from an undefined variable keeps the key it was taken with';
}

{
    my class OwnMapCandidate is Hash {
        multi method AT-KEY(Map:U: $key) { "own $key" }
    }
    is OwnMapCandidate.AT-KEY('a'), 'own a',
        'a Map:U candidate of a Hash subclass is called for its type object';
    my $held = OwnMapCandidate;
    is $held.AT-KEY('a'), 'own a',
        'a Map:U candidate of a Hash subclass is called for its type object in a container';
    my class OwnHashCandidate is Hash {
        multi method AT-KEY(Hash:U: $key) { "own $key" }
    }
    is OwnHashCandidate.AT-KEY('a'), 'own a',
        'a Hash:U candidate of a Hash subclass is called for its type object';
}

{
    my class Refusing is Hash {
        multi method ASSIGN-KEY(Refusing:D: Str:D \key, Mu \value) { Failure.new('refused') }
    }
    my Refusing $r;
    my $result = ($r<a> = 42);
    ok $result ~~ Failure,
        'a Failure the ASSIGN-KEY of a Hash subclass type object gives is returned';
    nok $r.defined,
        'a Hash subclass type object stays undefined when its ASSIGN-KEY refuses the key';
    $result.so;
}

{
    sub assign-into(Mu $h) { $h<a> = 1 }
    sub bind-into(Mu $h) { $h<a> := 1 }
    throws-like { assign-into(Hash[Int,Str]) }, X::AdHoc, message => /readonly/,
        'assigning to a key of a readonly object hash type object reports it as readonly';
    throws-like { bind-into(Hash[Int]) }, X::AdHoc, message => /readonly/,
        'binding a key of a readonly Hash[Int] type object reports it as readonly';
    my %bare := Hash[Int,Str];
    throws-like { %bare<a> = 1 }, X::Assignment::RO,
        'assigning to a key of an object hash type object outside a container refuses to modify it';
    my %plain := Hash[Int];
    throws-like { my $element := %plain<a>; $element = 1 }, X::Assignment::RO,
        'an element taken from a Hash[Int] type object outside a container refuses to modify it';
}

{
    my class RefusingBind is Hash {
        multi method BIND-KEY(RefusingBind:D: Str:D \key, Mu \value) is raw {
            key eq 'bad' ?? Failure.new('refused') !! callsame
        }
    }
    my RefusingBind $r;
    my $result = ($r<bad> := 42);
    ok $result ~~ Failure,
        'a Failure the BIND-KEY of a Hash subclass type object gives is returned';
    nok $r.defined,
        'a Hash subclass type object stays undefined when its BIND-KEY refuses the key';
    $result.so;
    my RefusingBind $p;
    my $ok  := $p<ok>;
    my $bad := $p<bad>;
    $ok = 1;
    throws-like { $bad = 2 }, Exception, message => 'refused',
        'an element taken from a Hash subclass type object throws when its BIND-KEY refuses the key';
}

{
    my class ValueBind is Hash {
        multi method BIND-KEY(ValueBind:D: Str:D \key, Mu \value) { callsame }
    }
    my ValueBind $h;
    my $element := $h<a>;
    $h = ValueBind.new;
    $element = Failure.new('stored');
    ok $h<a> ~~ Failure,
        'a Failure assigned to an element taken from a type object is stored through a BIND-KEY that is not raw';
    $h<a>.so;
}

{
    my class ValueKeys is Hash {
        multi method AT-KEY(ValueKeys:D: Str:D $key) is raw {
            self.EXISTS-KEY($key) ?? self.Hash::AT-KEY($key) !! 0
        }
    }
    my ValueKeys $v;
    my $element := $v<a>;
    $element = 42;
    is $v<a>, 42,
        'an element taken from a Hash subclass type object whose AT-KEY gives no container is bound into it';
}

{
    my Hash[Int(Str)] $h;
    my $element := $h<a>;
    $element = 'abc';
    ok $h<a> ~~ Failure,
        'a value that fails to coerce into an element a Hash[Int(Str)] type object vivifies is stored as a Failure';
    $h<a>.so;
}

{
    my Hash[Int,Str] %h{Str};
    %h<a><b> = 42;
    isa-ok %h<a>, Hash[Int,Str],
        'a typed hash value of a hash vivifies as that type';
    throws-like { %h<a><c> = 'x' }, X::TypeCheck::Assignment, expected => Int,
        'the typed hash value checks its value type';
    %h{'c';'d'} = 43;
    is %h<c><d>, 43,
        'a multidimensional subscript vivifies a typed hash value';
    throws-like { %h{'e';42} = 1 }, X::TypeCheck::Binding::Parameter,
        'a multidimensional subscript checks the key type of a typed hash value';
}

# vim: expandtab shiftwidth=4
