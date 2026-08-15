use Test;

# An `is copy` parameter of a native type declares a plain native lexical
# the body assigns to, and a definite type like `int:D` nominalizes to the
# same native base, so it takes the same path. The definedness constraint
# stays with the binder, where a native argument always satisfies it. A
# native step on a copy parameter lowers to the raw ops; a read-only or
# rw parameter keeps the routine call.

plan 18;

{
    sub f(int:D $x is copy) { $x = $x + 1; $x }
    is f(3), 4, 'an int:D copy parameter accepts assignment of a boxed Int';
}

{
    sub f(uint:D $x is copy) { $x = 5; $x }
    is f(3), 5, 'a uint:D copy parameter accepts assignment of a boxed Int';
}

{
    sub f(num:D $x is copy) { $x = 5e0; $x }
    is f(3e0), 5e0, 'a num:D copy parameter accepts assignment of a boxed Num';
}

{
    sub f(str:D $x is copy) { $x = 'b'; $x }
    is f('a'), 'b', 'a str:D copy parameter accepts assignment of a boxed Str';
}

{
    sub f(int:D $x is copy) { $x++; $x }
    is f(10), 11, 'an int:D copy parameter steps with postfix increment';
}

{
    sub f(int $i is copy) { $i++; $i++; --$i; $i }
    is f(10), 11, 'an int copy parameter steps with postfix and prefix forms';
}

{
    sub f(num $x is copy) { $x++; $x }
    is f(1e0), 2e0, 'a num copy parameter steps with postfix increment';
}

{
    sub f(int $i is copy, int $j) { $i += $j; $i }
    is f(10, 4), 14, 'an int copy parameter compound-steps from a parameter operand through the metaop';
}

{
    sub f(int $i is copy) { my int $j = 3; $i += $j; $i }
    is f(10), 13, 'an int copy parameter compound-steps from a native lexical operand';
}

{
    sub f(num $x is copy) { $x += 1.5e0; $x }
    is f(2e0), 3.5e0, 'a num copy parameter compound-steps from a float literal';
}

{
    sub f(int8 $i is copy) { $i++; $i }
    is f(3), 4, 'a narrow native copy parameter steps through the routine';
}

{
    sub f(int $i is copy = 7) { $i++; $i }
    is f(), 8, 'an int copy parameter with a default steps from the default';
}

{
    sub f(int $i is copy) { $i = $i + 1; $i }
    my int $x = 5;
    f($x);
    is $x, 5, 'assignment to an int copy parameter leaves the caller variable alone';
}

{
    sub f(int:D $x is rw) { $x = 9 }
    my int $x = 5;
    f($x);
    is $x, 9, 'an int:D rw parameter still writes back to the caller';
}

{
    sub f(int $i is rw) { $i++ }
    my int $x = 5;
    f($x);
    is $x, 6, 'an increment of an int rw parameter writes back to the caller';
}

{
    sub f(int:D $x is copy) { $x }
    dies-ok { f(Int) }, 'a type object argument to an int:D copy parameter is rejected';
}

{
    sub f(int $i) { $i++ }
    my $error = '';
    try f(3);
    $error = $!.Str if $!;
    ok $error.contains('mutable'),
        'postfix increment on a read-only native parameter still reports immutability';
}

{
    sub f(int:D $x) { $x }
    is f(3), 3, 'a plain int:D parameter still binds a boxed Int';
}

# vim: expandtab shiftwidth=4
