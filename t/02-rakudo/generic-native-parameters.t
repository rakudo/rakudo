use Test;

plan 27;

# A role method body compiles before the concrete types are known and
# treats its parameters and return values as boxed objects. A generic
# parameter or return type that instantiates to a native type therefore
# checks against the box type.

{
    my role R[::T] { method w(T $v) { $v + 1 } }
    my class C does R[int] {}
    is C.new.w(5), 6, 'a generic parameter instantiated with int accepts an Int argument';
    my int $x = 4;
    is C.new.w($x), 5, 'a generic parameter instantiated with int accepts a native int argument';
    throws-like { C.new.w("nope") }, X::TypeCheck::Binding::Parameter,
        'a generic parameter instantiated with int rejects a Str argument';
}

{
    my role R[::T] { multi method w(T $v) { $v + 1 } }
    my class C does R[int] {}
    is C.new.w(5), 6, 'a multi with a generic parameter instantiated with int dispatches';
}

{
    my role R[::T] {
        multi method w(T $v)   { "generic" }
        multi method w(Str $v) { "string"  }
    }
    my class C does R[int] {}
    is C.new.w(5), 'generic', 'an Int argument picks the generic candidate instantiated with int';
    is C.new.w("hi"), 'string', 'a Str argument picks the Str candidate beside an int instantiated one';
    my int $x = 3;
    is C.new.w($x), 'generic', 'a native int argument picks the generic candidate instantiated with int';
}

# A typevar bound to a definite native type boxes with the definiteness
# kept, in the signature and in the compiled parameter check alike.
{
    my role R[::T] { method w(T $v) { $v + 1 } }
    my class C does R[int:D] {}
    is C.new.w(5), 6, 'a generic parameter instantiated with int:D accepts a value';
    throws-like { C.new.w(Int) }, Exception,
        'a generic parameter instantiated with int:D rejects a type object';
}

{
    my role R[::T] { method w(--> T) { 5 } }
    my class C does R[int] {}
    is C.new.w, 5, 'a generic return type instantiated with int passes an Int return';
}

{
    my role R[::T] { method w(--> T) { "nope" } }
    my class C does R[int] {}
    throws-like { C.new.w }, X::TypeCheck::Return,
        'a generic return type instantiated with int rejects a Str return';
}

{
    my role R[::T] { method w(--> T:D) { 5 } }
    my class C does R[int] {}
    is C.new.w, 5, 'a definite generic return type instantiated with int passes a value';
}

{
    my role R[::T] { method w(--> T:D) { Int } }
    my class C does R[int] {}
    throws-like { C.new.w }, Exception,
        'a definite generic return type instantiated with int rejects a type object';
}

{
    my role R[::T] { method w(--> T:D) { "nope" } }
    my class C does R[int] {}
    throws-like { C.new.w }, X::TypeCheck::Return,
        'a definite generic return type instantiated with int rejects a Str value';
}

# Sized flavors box the same way as the full width types.
{
    my role R[::T] { method w(T $v) { $v + 1 } }
    my class C does R[int16] {}
    is C.new.w(5), 6, 'a generic parameter instantiated with int16 accepts an Int argument';
    throws-like { C.new.w("nope") }, X::TypeCheck::Binding::Parameter,
        'a generic parameter instantiated with int16 rejects a Str argument';
}

{
    my role R[::T] { method w(T $v) { $v + 1e0 } }
    my class C does R[num32] {}
    is C.new.w(1.5e0), 2.5e0, 'a generic parameter instantiated with num32 accepts a Num argument';
}

{
    my role R[::T] { method w(T $v) { $v + 1 } }
    my class C does R[uint8] {}
    is C.new.w(5), 6, 'a generic parameter instantiated with uint8 accepts an Int argument';
}

{
    my role R[::T] { method w(T:D $v) { $v + 1 } }
    my class C does R[int] {}
    is C.new.w(5), 6, 'a definite generic parameter instantiated with int accepts a value';
    throws-like { C.new.w(Int) }, X::Parameter::InvalidConcreteness,
        'a definite generic parameter instantiated with int rejects a type object';
}

{
    my role R[::T] { method w(T:U $v) { $v.^name } }
    my class C does R[int] {}
    is C.new.w(Int), 'Int', 'an undefined generic parameter instantiated with int accepts the box type object';
}

{
    my role R[::T] { method w(T $v) { $v + 1e0 } }
    my class C does R[num] {}
    is C.new.w(1.5e0), 2.5e0, 'a generic parameter instantiated with num accepts a Num argument';
}

{
    my role R[::T] { method w(T $v) { $v ~ "b" } }
    my class C does R[str] {}
    is C.new.w("a"), 'ab', 'a generic parameter instantiated with str accepts a Str argument';
}

# The instantiated signature reports the box type, since that is what it
# checks against.
{
    my role R[::T] { method w(T $v) { } }
    my class C does R[int] {}
    ok C.^lookup('w').signature.params[1].type =:= Int,
        'an int instantiated generic parameter reports Int as its type';
}

# A generic parameter instantiated with a non native type is untouched.
{
    my role R[::T] { method w(T $v) { $v * 2 } }
    my class C does R[Int] {}
    is C.new.w(5), 10, 'a generic parameter instantiated with Int still binds';
    ok C.^lookup('w').signature.params[1].type =:= Int,
        'an Int instantiated generic parameter reports Int as its type';
}

# A statically typed native parameter in a role keeps native binding.
{
    my role R { method w(int $v) { $v + 1 } }
    my class C does R {}
    my int $x = 4;
    is C.new.w($x), 5, 'a role method parameter declared int still binds a native argument';
}

# vim: expandtab shiftwidth=4
