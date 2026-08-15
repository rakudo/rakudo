use Test;

plan 46;

# A role method accessing a generically typed attribute compiles before the
# concrete type is known, so it uses object access ops and is never
# recompiled. Native storage cannot serve those accesses, so an attribute
# whose generic type instantiates to a native type stores under the box
# type instead.

{
    my role R[::T] {
        has T $!x;
        method step()     { $!x++; $!x }
        method stepdown() { $!x--; $!x }
        method bump()     { ++$!x }
        method set($v)    { $!x = $v; $!x }
        method add($v)    { $!x += $v; $!x }
        method read()     { $!x }
    }
    my class C does R[int] {}

    my $c = C.new;
    is $c.step, 1, 'postfix increment works on a generic attribute instantiated with int';
    is $c.stepdown, 0, 'postfix decrement works on a generic attribute instantiated with int';
    is $c.bump, 1, 'prefix increment works on a generic attribute instantiated with int';
    is $c.set(5), 5, 'assignment works on a generic attribute instantiated with int';
    is $c.add(3), 8, 'compound assignment works on a generic attribute instantiated with int';
    is $c.read, 8, 'a read returns the stored value';
    isa-ok $c.read, Int, 'a read of an int instantiated generic attribute returns an Int';
}

{
    my role R[::T] { has T $!x; method w() { $!x++; $!x } }
    my class C does R[num] {}
    my $c = C.new;
    $c.w;
    is $c.w, 2e0, 'postfix increment works on a generic attribute instantiated with num';
}

{
    my role R[::T] { has T $!x; method w() { $!x = "a"; $!x ~= "b"; $!x } }
    my class C does R[str] {}
    is C.new.w, "ab", 'assignment and concatenation work on a generic attribute instantiated with str';
}

{
    my role R[::T] { has T $!x; method w() { $!x++; $!x } }
    my class C does R[uint] {}
    is C.new.w, 1, 'postfix increment works on a generic attribute instantiated with uint';
}

{
    my role R[::T] { has T $!x; method w() { $!x += 3; $!x } }
    my class C does R[int8] {}
    is C.new.w, 3, 'compound assignment works on a generic attribute instantiated with int8';
}

# The instantiated attribute reports the box type, since that is what it
# stores.
{
    my role R[::T] { has T $!x }
    my class C does R[int] {}
    ok C.^attributes.first(*.name eq '$!x').type =:= Int,
        'an int instantiated generic attribute reports Int as its type';
}

{
    my role R[::T] { has T $!x; method w() { $!x } }
    my class C does R[int] {}
    ok C.new.w =:= Int, 'an untouched int instantiated generic attribute reads as the Int type object';
}

# The box type still constrains assignment.
{
    my role R[::T] { has T $!x; method w($v) { $!x = $v } }
    my class C does R[int] {}
    throws-like { C.new.w("nope") }, X::TypeCheck,
        'assigning a non-numeric value to an int instantiated generic attribute dies';
}

# Nil assignment resets to the box type object.
{
    my role R[::T] { has T $!x; method w() { $!x = 5; $!x = Nil; $!x } }
    my class C does R[int] {}
    ok C.new.w =:= Int, 'assigning Nil resets an int instantiated generic attribute to Int';
}

# A definite generic type over a native type boxes the same way, keeping
# the definiteness constraint.
{
    my role R[::T] { has T:D $!x is built; method w() { $!x++; $!x } }
    my class C does R[int] {}
    is C.new(x => 1).w, 2, 'a definite generic attribute instantiated with int steps';
}

{
    my role R[::T] { has T:D $!x is built }
    my class C does R[int] {}
    ok C.^attributes.first(*.name eq '$!x').type =:= Int:D,
        'a definite int instantiated generic attribute reports Int:D as its type';
    throws-like { C.new(x => Int) }, Exception,
        'a definite int instantiated generic attribute rejects a type object';
}

# Construction time paths reach the boxed storage as well.
{
    my role R[::T] { has T $.x = 3 }
    my class C does R[int] {}
    is C.new.x, 3, 'a default value initializes an int instantiated generic attribute';
    is C.new(x => 5).x, 5, 'a named constructor argument initializes an int instantiated generic attribute';
}

{
    my role R[::T] { has T $.x is rw }
    my class C does R[int] {}
    my $c = C.new;
    $c.x = 7;
    is $c.x, 7, 'the generated rw accessor writes an int instantiated generic attribute';
}

{
    my role R[::T] { has T $!x; method w() { $!x++; $!x } }
    my class C does R[int] {}
    my $a = C.new;
    my $b = C.new;
    $a.w; $a.w;
    is $b.w, 1, 'each object steps its own copy of an int instantiated generic attribute';
    is $a.w, 3, 'stepping one object does not disturb another';
}

# An attribute with a statically known native type keeps native storage
# when the role supplying it composes into a class.
{
    my role R { has int $!x = 1; method w() { $!x++; $!x } }
    my class C does R {}
    ok C.^attributes.first(*.name eq '$!x').type =:= int,
        'a role attribute declared int keeps its native type';
    is C.new.w, 2, 'postfix increment works on a role attribute declared int';
}

# The instantiation reaches attributes through nested role topologies.
{
    my role R[::T] { has T $!x; method w() { $!x++; $!x } }
    my role S[::T] does R[T] { method v() { self.w + 1 } }
    my class C does S[int] {}
    is C.new.w, 1, 'a generic attribute survives a role to role generic pass through instantiated with int';
    is C.new.v, 2, 'a method in the outer role reaches the passed through generic attribute';
}

{
    my role R[::T] { has T $!x; method w() { $!x++; $!x } }
    my role S does R[int] { method v() { self.w + 1 } }
    my class C does S {}
    is C.new.w, 1, 'a generic attribute instantiated with int inside an intermediate role steps';
    is C.new.v, 2, 'a method in the consuming role reaches the instantiated generic attribute';
}

# Punned and runtime mixed in roles compose the boxed storage as well.
{
    my role R[::T] { has T $.x; method w() { $!x++; $!x } }
    is R[int].new.w, 1, 'a punned role steps a generic attribute instantiated with int';
    is R[int].new(x => 5).x, 5, 'a punned role constructs a generic attribute instantiated with int';
    ok R[int].new.x =:= Int, 'an untouched generic attribute on a pun reads as the Int type object';
}

{
    my role R[::T] { has T $!x; method w() { $!x++; $!x } }
    my class C {}
    my $c = C.new does R[int];
    is $c.w, 1, 'a runtime does mixin steps a generic attribute instantiated with int';
    my $d = C.new but R[int];
    is $d.w, 1, 'a runtime but mixin steps a generic attribute instantiated with int';
}

# Sized flavors box the same way as the full width types.
{
    my role R[::T] { has T $!x; method w() { $!x++; $!x } }
    my class C does R[int16] {}
    is C.new.w, 1, 'postfix increment works on a generic attribute instantiated with int16';
}

{
    my role R[::T] { has T $!x; method w() { $!x += 2e0; $!x } }
    my class C does R[num32] {}
    is C.new.w, 2e0, 'compound assignment works on a generic attribute instantiated with num32';
}

# An undefined constrained generic attribute stores the box type object.
{
    my role R[::T] { has T:U $!x is built; method w() { $!x.^name } }
    my class C does R[int] {}
    is C.new(x => Int).w, 'Int', 'an undefined generic attribute instantiated with int stores the box type object';
    throws-like { C.new(x => 5) }, Exception,
        'an undefined generic attribute instantiated with int rejects a concrete value';
}

# A method compiled before the attribute declaration is known picks up the
# native type once the class closes.
{
    my class C { method m() { $!x++; $!x }; has int $!x = 1 }
    is C.new.m, 2, 'a method ahead of a native attribute declaration steps it';
}

{
    my class C { method m() { $!n += 1.5e0; $!n }; has num $!n = 1e0 }
    is C.new.m, 2.5e0, 'a method ahead of a native num attribute declaration compound assigns it';
}

{
    my class C { method m() { $!s ~= "b"; $!s }; has str $!s = "a" }
    is C.new.m, 'ab', 'a method ahead of a native str attribute declaration concatenates onto it';
}

{
    my class C { method m() { $!u++; $!u }; has uint8 $!u = 1 }
    is C.new.m, 2, 'a method ahead of a native uint8 attribute declaration steps it';
}

{
    my role R { has int $!x = 1 }
    my class C does R { method m() { $!x++; $!x } }
    is C.new.m, 2, 'a class body method steps a native attribute supplied by a role';
}

{
    my role R[::T] { has T $!x }
    my class C does R[int] { method m() { $!x++; $!x } }
    is C.new.m, 1, 'a class body method steps a generic role attribute instantiated with int';
}

# A generic attribute instantiated with a non native type is untouched by
# the native boxing.
{
    my role R[::T] { has T $!x; method w() { $!x++; $!x } }
    my class C does R[Int] {}
    is C.new.w, 1, 'postfix increment works on a generic attribute instantiated with Int';
    ok C.^attributes.first(*.name eq '$!x').type =:= Int,
        'an Int instantiated generic attribute reports Int as its type';
}

# vim: expandtab shiftwidth=4
