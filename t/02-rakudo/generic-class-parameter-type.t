use Test;

plan 19;

# https://github.com/rakudo/rakudo/issues/5805
# Parameterizing Array or Hash with a still-generic T produces a generic
# class rather than a type capture or a parametric role curry. Binding a
# parameter of such a type must instantiate it from the type environment.

sub array-taker(::T $a, Array[T] $b) { $b }
{
    my Int @a = 666;
    my $bound = array-taker(42, @a);
    ok $bound.WHAT =:= Array[Int], 'Array[T] parameter instantiates to Array[Int] for an Int argument pair';
    is-deeply $bound.List, (666,), 'Array[T] parameter binds the passed array contents';
}
{
    my Str @a = 'x';
    ok array-taker('s', @a).WHAT =:= Array[Str], 'Array[T] parameter instantiates to Array[Str] for a Str argument pair';
}
{
    my Str @a = 'x';
    throws-like { array-taker(42, @a) }, X::TypeCheck::Binding::Parameter,
        'Array[T] parameter rejects an array whose type does not match the instantiated T';
}

sub hash-taker(::T $a, Hash[T] $b) { $b }
{
    my Int %h = a => 666;
    my $bound = hash-taker(42, %h);
    ok $bound.WHAT =:= Hash[Int], 'Hash[T] parameter instantiates to Hash[Int] for an Int argument pair';
    is-deeply $bound<a>, 666, 'Hash[T] parameter binds the passed hash contents';
}
{
    my Int %h = a => 666;
    throws-like { hash-taker('s', %h) }, X::TypeCheck::Binding::Parameter,
        'Hash[T] parameter rejects a hash whose type does not match the instantiated T';
}

sub positional-taker(::T $a, Positional[T] $b) { $b }
{
    my Int @a = 666;
    ok positional-taker(42, @a).WHAT =:= Array[Int], 'Positional[T] parameter still binds an Array[Int] argument';
}

sub capture-taker(::T $a, T $b) { $b }
is capture-taker(42, 43), 43, 'plain T parameter still binds a value of the captured type';
throws-like { capture-taker(42, 'str') }, X::TypeCheck::Binding::Parameter,
    'plain T parameter still rejects a value outside the captured type';

{
    role ArrayMethod[::T] { method m(Array[T] $x) { $x } }
    my class WithInt does ArrayMethod[Int] { }
    my Int @a = 1;
    ok WithInt.m(@a).WHAT =:= Array[Int], 'Array[T] parameter on a role method binds after concretization';
    my Str @b = 'x';
    throws-like { WithInt.m(@b) }, X::TypeCheck::Binding::Parameter,
        'Array[T] parameter on a role method rejects a mismatched array after concretization';
}

{
    role PositionalMethod[::T] { method m(Positional[T] $x) { $x } }
    my class PosWithInt does PositionalMethod[Int] { }
    my Int @a = 1;
    ok PosWithInt.m(@a).WHAT =:= Array[Int], 'Positional[T] parameter on a role method binds after concretization';
}

{
    role CaptureMethod[::T] { method m(T $x) { $x } }
    my class CapWithInt does CaptureMethod[Int] { }
    is CapWithInt.m(5), 5, 'plain T parameter on a role method still binds after concretization';
}

# Multi dispatch trial-binds a generic candidate, so it must also
# instantiate generic class parameter types to decide a match.
multi multi-taker(::T $a, Array[T] $b) { 'generic' }
multi multi-taker($a, $b) { 'fallback' }
{
    my Int @a = 666;
    is multi-taker(42, @a), 'generic', 'multi dispatch picks the Array[T] candidate for a matching argument pair';
    my Str @b = 'x';
    is multi-taker(42, @b), 'fallback', 'multi dispatch falls back when the array type does not match T';
}

multi lone-taker(::T $a, Array[T] $b) { 'generic' }
{
    my Str @a = 'x';
    throws-like { lone-taker(42, @a) }, X::Multi::NoMatch,
        'multi dispatch reports no match when the only candidate has a mismatched Array[T] parameter';
}

{
    role MultiMethod[::T] {
        multi method m(Array[T] $x) { 'typed' }
        multi method m($x) { 'other' }
    }
    my class MultiWithInt does MultiMethod[Int] { }
    my Int @a = 1;
    is MultiWithInt.m(@a), 'typed', 'multi method on a role picks the Array[T] candidate after concretization';
    is MultiWithInt.m('s'), 'other', 'multi method on a role falls back for a non-array argument';
}

# vim: expandtab shiftwidth=4
