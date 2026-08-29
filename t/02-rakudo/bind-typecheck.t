use nqp;
use Test;
use MONKEY-SEE-NO-EVAL;

# An infix bind checks its source against the target container's bind
# constraint and deconts the source when the target has the @ or %
# sigil. The check covers types that wrap another type node, such as
# definite, coercion, and parameterized types, and also applies when
# the target is a typed is copy parameter rebound in the routine body.
# A bind declaration checks the same way and is refused on a shaped
# array declaration, whose shape the bind would discard. A generic
# constraint is checked against the type it is instantiated with.

my $rakuast := nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';

plan 47;

{
    my Int $y = 42;
    my Int:D $x = 1;
    $x := $y;
    is $x, 42, 'can bind a defined value to a variable with a definite type';
}

throws-like {
    my Int $y;
    my Int:D $x = 1;
    $x := $y;
}, X::TypeCheck::Binding,
    'binding an undefined value to a variable with a definite type throws';

{
    my $y = "9";
    my Int(Str) $x = 5;
    $x := $y;
    is $x.^name, 'Str', 'can bind an accepted value to a variable with a coercion type';
}

throws-like {
    my $y = 4.5;
    my Int(Str) $x = 5;
    $x := $y;
}, X::TypeCheck::Binding,
    'binding a value outside a coercion type to a variable with a coercion type throws';

{
    my @a := Array[Int].new(1, 2);
    my Array[Int] $x;
    $x := @a;
    is-deeply $x, Array[Int].new(1, 2),
        'can bind a matching value to a variable with a parameterized type';
}

throws-like {
    my @a = 1, 2;
    my Array[Int] $x;
    $x := @a;
}, X::TypeCheck::Binding,
    'binding an untyped array to a variable with a parameterized type throws';

throws-like {
    my Int $x = 1;
    $x := "s";
}, X::TypeCheck::Binding,
    'binding a value of the wrong type to a typed scalar throws';

{
    my Int $x = 1;
    my $y = 5;
    $x := $y;
    is $x, 5, 'a valid bind to a typed scalar takes the new value';
    $y = 7;
    is $x, 7, 'a scalar bind aliases the source container';
    $x = 9;
    is $y, 9, 'assignment through a rebound scalar reaches the source container';
}

throws-like {
    my @a;
    @a := 5;
}, X::TypeCheck::Binding,
    'binding a non-Positional value to an @ sigil variable throws';

throws-like {
    my Int @a;
    my @b = "x", "y";
    @a := @b;
}, X::TypeCheck::Binding,
    'binding an untyped array to a typed @ sigil variable throws';

{
    my @b = 1, 2;
    my $s = @b;
    my @a;
    @a := $s;
    is @a.VAR.^name, 'Array',
        'binding a Scalar-held array to an @ sigil variable deconts the source';
}

{
    my @b = 1, 2;
    my @a;
    @a := @b;
    @b.push(3);
    is @a.elems, 3, 'binding an array to an @ sigil variable aliases it';
}

throws-like {
    my @a is Array[Int];
    @a := ["x"];
}, X::TypeCheck::Binding,
    'binding an untyped array to an @ sigil variable with an explicit container base throws';

{
    my @a is Array[Int];
    @a := Array[Int].new(1, 2);
    is-deeply @a, Array[Int].new(1, 2),
        'can bind a matching array to an @ sigil variable with an explicit container base';
}

throws-like {
    my %h is Hash[Int];
    my %b = a => "x";
    %h := %b;
}, X::TypeCheck::Binding,
    'binding an untyped hash to a % sigil variable with an explicit container base throws';

{
    my %h is Hash[Int];
    %h := Hash[Int].new((a => 1));
    is %h<a>, 1,
        'can bind a matching hash to a % sigil variable with an explicit container base';
}

throws-like {
    my %h;
    %h := 5;
}, X::TypeCheck::Binding,
    'binding a non-Associative value to a % sigil variable throws';

{
    my %b = a => 1;
    my $s = %b;
    my %h;
    %h := $s;
    is %h.VAR.^name, 'Hash',
        'binding a Scalar-held hash to a % sigil variable deconts the source';
}

throws-like {
    my Int %h;
    my %b = a => "x";
    %h := %b;
}, X::TypeCheck::Binding,
    'binding an untyped hash to a typed % sigil variable throws';

throws-like {
    sub f(Int $x is copy) { $x := "s" }
    f(42);
}, X::TypeCheck::Binding,
    'rebinding a typed is copy parameter to the wrong type throws';

{
    sub f(Int $x is copy) { $x := 99; $x }
    is f(42), 99, 'a valid rebind of a typed is copy parameter takes the new value';
}

{
    sub f($x is copy) { $x := "s"; $x }
    is f(42), "s", 'an untyped is copy parameter can be rebound to any type';
}

throws-like {
    sub f(Int @a is copy) { @a := ["x"] }
    my Int @a = 1, 2;
    f(@a);
}, X::TypeCheck::Binding,
    'rebinding a typed @ sigil is copy parameter to the wrong type throws';

throws-like {
    sub f() { state Int $x; $x := "s" }
    f;
}, X::TypeCheck::Binding,
    'binding a value of the wrong type to a typed state variable throws';

throws-like {
    my UInt $x = 1;
    my $y = -5;
    $x := $y;
}, X::TypeCheck::Binding,
    'binding a value outside a subset type to a variable with a subset type throws';

{
    my UInt $x = 1;
    my $y = 5;
    $x := $y;
    is $x, 5, 'can bind an accepted value to a variable with a subset type';
}

{
    my role R[::T] { method m(T $x is copy, T $y) { $x := $y; $x } }
    ok R[Int] ~~ R, 'a role body with a bind to a generic typed variable compiles';
}

throws-like {
    my role R[::T] { method m(T $x is copy, $y) { $x := $y } }
    R[Int].new.m(1, "s");
}, X::TypeCheck::Binding,
    'rebinding a generic typed is copy parameter to the wrong type throws';

throws-like {
    my Int $x := "s";
}, X::TypeCheck::Binding,
    'a bind declaration of a typed scalar checks the bound value';

{
    my $y = 5;
    my Int $x := $y;
    is $x, 5, 'a valid bind declaration of a typed scalar takes the value';
    $y = 7;
    is $x, 7, 'a bind declaration of a scalar aliases the source container';
}

throws-like {
    my Int @a := ["x"];
}, X::TypeCheck::Binding,
    'a bind declaration of a typed @ sigil variable checks the bound value';

{
    my Int @a := Array[Int].new(1, 2);
    is-deeply @a, Array[Int].new(1, 2),
        'a valid bind declaration of a typed @ sigil variable takes the value';
}

{
    my @b = 1, 2;
    my $s = @b;
    my @a := $s;
    is @a.VAR.^name, 'Array',
        'a bind declaration of an @ sigil variable deconts the source';
}

throws-like {
    my Int %h := { a => "x" };
}, X::TypeCheck::Binding,
    'a bind declaration of a typed % sigil variable checks the bound value';

{
    my Int %h := Hash[Int].new((a => 1));
    is %h<a>, 1,
        'a valid bind declaration of a typed % sigil variable takes the value';
}

throws-like {
    my $x of Int;
    $x := "s";
}, X::TypeCheck::Binding,
    'binding a value of the wrong type to a variable typed by an of trait throws';

{
    our $x := 5;
    is $x, 5, 'a bind declaration of an our variable takes the value';
}

{
    class WithKeys does Associative[Any, Int] { method AT-KEY($k) { 42 } }
    my %h{Int} := WithKeys.new;
    is %h{1}, 42, 'a keyed hash declaration can take a binding initializer';
}

{
    class ShapedAttr { has @.a[2] }
    is-deeply ShapedAttr.new.a.shape, (2,),
        'a shaped attribute declaration without a binding initializer keeps its shape';
}

{
    my @a[2] = 1, 2;
    is @a[1], 2, 'a shaped array declaration with an assignment initializer works';
}

throws-like q[my @a[2] := [1,2]],
    X::Bind,
    'a bind declaration of a shaped variable is refused at compile time';

if $rakuast {
    is EVAL(q:to/CODE/), 5,
        my role R[::T] { method m(T $v) { my T $x := $v; $x } }
        R[Int].new.m(5)
        CODE
        'a bind declaration with a generic type takes the bound value';

    throws-like q:to/CODE/, X::TypeCheck::Binding,
        my role R[::T] { method m($v) { my T $x := $v } }
        R[Int].new.m("s")
        CODE
        'a bind declaration with a generic type checks the bound value';

    is EVAL(q:to/CODE/), 2,
        my role R[::T] { method m(T $x is copy, T $y) { $x := $y; $x } }
        R[Int].new.m(1, 2)
        CODE
        'a generic typed is copy parameter can be rebound to a matching value';
}
else {
    skip 'the legacy frontend asserts a generic bind against the un-instantiated type', 3;
}

# vim: expandtab shiftwidth=4
