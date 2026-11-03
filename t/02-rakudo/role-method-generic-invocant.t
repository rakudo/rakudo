use Test;

plan 10;

# A private method call is resolved in the caller's package, so a role
# method can receive an invocant of another class doing the same role.
# The implicit $?CLASS invocant takes no runtime type check, matching the
# legacy frontend. RakuAST used to check it against the caller's own
# concretization and fail to bind.
{
    my role Element {
        method !resolve() { 'resolved by ' ~ self.^name }
        method resolve-in(Element $other) { $other!resolve() }
    }
    my class Doc does Element { }
    my class Index does Element { }
    is Doc.new.resolve-in(Index.new), 'resolved by Index',
        'a role private method called on an object of another class binds its invocant';
}

# The same holds for a concretized method invoked directly as a code object.
{
    my role R { method !priv() { self.^name } }
    my class A does R { }
    my class B does R { }
    is A.^find_private_method('priv')(B.new), 'B',
        'a concretized private method accepts an invocant of another class doing the role';
}

# Concretization still instantiates $?CLASS in the introspectable signature.
{
    my role R { method !priv() { } }
    my class A does R { }
    is A.^find_private_method('priv').signature.params[0].type.^name, 'A',
        'the concretized private method still advertises the class as its invocant type';
}

# Public role methods take the same implicit invocant.
{
    my role R { method pub() { self.^name } }
    my class A does R { }
    my class B does R { }
    is A.^find_method('pub')(B.new), 'B',
        'a concretized public method accepts an invocant of another class doing the role';
}

# An explicitly written but untyped invocant has no type to check.
{
    my role R { method !priv($self:) { $self.^name } }
    my class A does R { }
    my class B does R { }
    is A.^find_private_method('priv')(B.new), 'B',
        'an untyped explicit invocant binds an object of another class';
}

# An invocant type written in the signature keeps its runtime check even
# when it is the same generic ::?CLASS the implicit invocant would get.
{
    my role R { method !priv(::?CLASS $self:) { } }
    my class A does R { }
    my class B does R { }
    throws-like { A.^find_private_method('priv')(B.new) },
        X::TypeCheck::Binding::Parameter,
        'an explicitly ::?CLASS-typed invocant still rejects another class';
}

# A type captured by the role's signature keeps its runtime check on
# non-invocant parameters.
{
    my role R[::T] { method !take(T $x) { $x } }
    my class A does R[Int] { }
    is A.^find_private_method('take')(A.new, 42), 42,
        'a role type-capture parameter still binds a matching argument';
    throws-like { A.^find_private_method('take')(A.new, 'nope') },
        X::TypeCheck::Binding::Parameter,
        'a role type-capture parameter still rejects a mismatched argument';
}

# A signature the lowered binding cannot handle falls back to the full
# binder, which enforces the instantiated invocant type on both frontends.
{
    my role R { method !destructure([$a, $b]) { $a + $b } }
    my class A does R { }
    my class B does R { }
    throws-like { A.^find_private_method('destructure')(B.new, [1, 2]) },
        X::TypeCheck::Binding::Parameter,
        'a sub-signature method rejects a cross-class invocant in the full binder';
}

# Methods declared directly in a class have a concrete invocant type and
# keep their runtime check.
{
    my class A { method m() { 'hi' } }
    my class B { }
    throws-like { A.^find_method('m')(B.new) },
        X::TypeCheck::Binding::Parameter,
        'a class method still rejects an invocant of an unrelated class';
}
