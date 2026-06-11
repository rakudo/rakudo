use Test;

plan 21;

# Assigning the result of a call to a native-typed container coerces with
# the unbox that matches the callee's declared native return type. A routine
# that returns native `int` widens to `num` on assignment. This must work for
# subs, for multis, and for the built-in operators, including nested and
# parenthesized expressions, whether or not the call gets inlined.

{
    sub f(int $a, int $b --> int) { $a * $b }
    my int $a = 3; my int $b = 4;
    my num $y; $y = f($a, $b);
    is $y, 12e0, 'native-return sub widens to num on assignment';
}

{
    multi g(int $a, int $b --> int) { $a * $b }
    my int $a = 3; my int $b = 4;
    my num $y; $y = g($a, $b);
    is $y, 12e0, 'native-return multi widens to num on assignment';
}

{
    my int $a = 3; my int $b = 4;
    my num $y; $y = $a * $b;
    is $y, 12e0, 'int * int widens to num on assignment';
}

{
    my int $a = 3; my int $b = 4;
    my num $y; $y = $a + $b;
    is $y, 7e0, 'int + int widens to num on assignment';
}

{
    my int $two = 2; my int $i = 5; my int $one = 1;
    my num $y; $y = $two * $i - $one;
    is $y, 9e0, 'nested native int operators widen to num';
}

# A literal operand compiles as the boxed value, so the dispatch analysis
# does not assume the native candidate answers at runtime; the result stays
# boxed and a native-typed target keeps requiring explicit coercion.
{
    my int $i = 5;
    is 2 * $i, 10, 'literal and native int mix computes the boxed result';
    my num $y;
    dies-ok { $y = 2 * $i }, 'literal and native int mix is not assumed native';
}

{
    my int $a = 3; my int $b = 4; my int $c = 2; my int $d = 3;
    my num $y; $y = ($a * $b) + ($c * $d);
    is $y, 18e0, 'parenthesized native results widen to num';
}

# A statement modifier changes what grouping parentheses produce, so they
# forward no type.
{
    sub elems-of(Iterable $x) { $x.elems }
    my int $i = 5;
    is elems-of(($i for 1..3)), 3, 'a parenthesized loop modifier stays an Iterable';
}

{
    my int $a = 2; my int $b = 3; my int $c = 4;
    my num $y; $y = $a * $b * $c;
    is $y, 24e0, 'chained native multiply widens to num';
}

{
    my int $i = 5;
    my num $y; $y = -$i * $i;
    is $y, -25e0, 'native prefix result participates in native dispatch';
}

{
    my int $a = 2; my int $b = 3;
    my num $y; $y = $a ** $b;
    is $y, 8e0, 'all-native exponentiation widens to num';
}

{
    my int $i = 5;
    my num $y; $y = -$i;
    is $y, -5e0, 'native prefix negate widens to num';
}

# Going the other way, a native num result narrows to a native int container.
{
    my num $a = 3.5e0; my num $b = 0.5e0;
    my int $i; $i = $a + $b;
    is $i, 4, 'num + num narrows to int on assignment';
}

# A native str return assigns to a native str container.
{
    sub st(--> str) { "x" }
    my str $s; $s = st();
    is $s, "x", 'native str return assigns to str';
}

# An `is rw` routine with a native return type yields an assignable container,
# not a value to unbox. Calling it as an lvalue must keep working.
{
    my int $g = 1;
    sub acc() is rw returns int { $g }
    acc() = 99;
    is $g, 99, 'rw routine with native return stays an lvalue';
}

# Assigning a value whose static type is a boxed numeric stays strict. It is
# not silently coerced; an explicit coercion is required.
{
    my Int $n = 5;
    my num $y;
    dies-ok { $y = $n }, 'boxed Int to native num still requires explicit coercion';
}

# A routine that declares a boxed return type is not a native result; assigning
# it to a native container stays strict.
{
    sub boxed(--> Int) { 5 }
    my num $y;
    dies-ok { $y = boxed() }, 'boxed Int return to native num still requires coercion';
}

# A subset return type is not a native result either.
{
    subset Positive of Int where * > 0;
    sub sub-ret(--> Positive) { 5 }
    my num $y;
    dies-ok { $y = sub-ret() }, 'subset return to native num still requires coercion';
}

# A routine with a native return type may still escape with a Failure (or
# Nil), which is exempt from return-type checks. A context that wants the
# boxed object must receive it without unboxing.
{
    my int $i = 4;
    my $r = $i div 0;
    is-deeply $r.defined, False, 'div by zero on native ints stays a soft Failure';
}

{
    multi gfail(int $a --> int) { $a > 0 ?? $a !! fail "neg" }
    my int $i = -3;
    my $x = gfail($i);
    is $x // 'default', 'default', 'Failure from a native-return multi flows through //';
}
