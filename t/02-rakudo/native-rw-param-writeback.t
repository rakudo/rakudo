use Test;

# A rw parameter of native type binds the caller's native reference, so
# an assignment to the parameter writes back through it. The rw-ness can
# come from an is rw trait or from the <-> block starter. In a <-> block
# an is copy trait keeps the parameter rw, since the block starter sets
# the rw flag on every parameter.

plan 31;

{
    my int @a = 1, 2, 3;
    for @a <-> int $i { $i++ }
    is-deeply @a, array[int].new(2, 3, 4),
      'a for loop over a native int array writes back through <-> int';
}

{
    my uint @a = 1, 2, 3;
    for @a <-> uint $u { $u++ }
    is-deeply @a, array[uint].new(2, 3, 4),
      'a for loop over a native uint array writes back through <-> uint';
}

{
    my num @a = 1e0, 2e0;
    for @a <-> num $n { $n = $n * 2e0 }
    is-deeply @a, array[num].new(2e0, 4e0),
      'a for loop over a native num array writes back through <-> num';
}

{
    my str @a = 'a', 'b';
    for @a <-> str $s { $s = $s ~ '!' }
    is-deeply @a, array[str].new('a!', 'b!'),
      'a for loop over a native str array writes back through <-> str';
}

{
    my int @a = 1, 2, 3, 4;
    for @a <-> int $i, int $j { $i++; $j-- }
    is-deeply @a, array[int].new(2, 1, 4, 3),
      'a for loop taking two <-> int parameters writes back through both';
}

{
    my &block = <-> int $i { $i = 42 };
    my int $x = 1;
    block($x);
    is $x, 42,
      'calling a <-> block with a native int variable writes back';
    ok &block.signature.params[0].rw,
      'a native parameter of a <-> block introspects as rw';
}

{
    my int @a = 1, 2, 3;
    for @a <-> int $i is copy { $i++ }
    is-deeply @a, array[int].new(2, 3, 4),
      'an is copy native parameter of a <-> block still writes back';
}

{
    my @a = 1, 2, 3;
    for @a <-> $i is copy { $i++ }
    is-deeply @a, [2, 3, 4],
      'an is copy ordinary parameter of a <-> block still writes back';
}

{
    sub f(int $i is rw) { $i = 9 }
    my int $x = 1;
    f($x);
    is $x, 9,
      'a sub with a native int is rw parameter writes back';
}

{
    sub f(int $i is copy) { $i++ }
    my int $x = 1;
    f($x);
    is $x, 1,
      'a sub with a native int is copy parameter does not write back';
}

{
    my @a = 1, 2, 3;
    for @a <-> $i { $i++ }
    is-deeply @a, [2, 3, 4],
      'a for loop over an ordinary array writes back through a <-> parameter';
}

# A variable declarator signature also flags its parameters rw, but
# its targets declare ordinary variables. A native one must stay a
# plain lexical so assignment reaches it.
{
    my (int $a, num $b);
    $a = 42;
    $b = 4e2;
    is $a, 42,
      'a native int declared in a declarator signature accepts assignment';
    is $b, 4e2,
      'a native num declared in a declarator signature accepts assignment';
}

{
    my (int $a) = 5;
    is $a, 5,
      'a declarator signature holding one native int takes its initializer';
}

# A short circuit operator that yields its native operand yields it as the
# reference, which an rw or raw parameter binds.
{
    sub f(int $x is rw) { $x = $x + 10 }
    my int $i = 1; f($i || 5);
    is $i, 11, 'a native int passed through || to an rw parameter writes back';
}
{
    sub f(int $x is rw) { $x = $x + 10 }
    my int $i = 0; f($i && 5);
    is $i, 10, 'a native int passed through && to an rw parameter writes back';
}
{
    sub f(\x) { x = 9 }
    my int $i = 1; f($i || 5);
    is $i, 9, 'a native int passed through || to a raw parameter writes back';
}
{
    sub f(str $x is rw) { $x = $x ~ '!' }
    my str $s = 'a'; f($s || 'b');
    is $s, 'a!', 'a native str passed through || to an rw parameter writes back';
}
{
    sub f(num $x is rw) { $x = $x + 1e0 }
    my num $n = 1e0; f($n || 5e0);
    is $n, 2e0, 'a native num passed through || to an rw parameter writes back';
}
{
    sub f(int $x is rw) { $x = $x + 10 }
    sub g(int $p is rw) { f($p || 5) }
    my int $i = 1; g($i);
    is $i, 11, 'an rw native parameter passed through || to an rw parameter writes back';
}

{
    class C { method m(int $x is rw) { $x = $x + 10 } }
    my int $i = 1; C.m($i || 5);
    is $i, 11, 'a native operand of || passed to an rw method parameter writes back';
}
{
    class D { method m(\x) { x = 9 } }
    my int $i = 1; D.m($i || 5);
    is $i, 9, 'a native operand of || passed to a raw method parameter writes back';
}
{
    sub f($x is rw) { $x = 9 }
    my int $i = 1; f($i || 5);
    is $i, 9, 'a native operand of || passed to a boxed rw parameter writes back';
}
{
    sub inner(int $x is rw) { $x = $x + 10 }
    sub outer(int $p is rw) { inner($p || 5, ); $p = $p + 1 }
    my int $i = 1; outer($i);
    is $i, 12, 'an rw native parameter passed on through || writes back through both calls';
}
{
    my int $i = 1; f-later($i || 5);
    is $i, 9, 'a native operand of || reaches an rw candidate declared after the call';
    multi sub f-later(int $x is rw) { $x = 9 }
}

# A native value without a container is rejected the same way a boxed
# one is, whichever shape the call passed it in.
throws-like { for 1..3 <-> int $i { } },
  X::Parameter::RW,
  message => /'writable container'/,
  got => 1,
  'a for loop over a range of values rejects a <-> int parameter';
{
    sub takes-int(int $x is rw) { }
    sub takes-num(num $x is rw) { }
    sub takes-str(str $x is rw) { }
    my int $i = 1;
    my num $n = 1e0;
    my str $s = "a";
    throws-like { takes-int($i + 1) }, X::Parameter::RW, got => 2,
      'a native int expression is rejected by an rw int parameter';
    throws-like { takes-num($n + 1e0) }, X::Parameter::RW, got => 2e0,
      'a native num expression is rejected by an rw num parameter';
    throws-like { takes-str($s ~ "b") }, X::Parameter::RW, got => "ab",
      'a native str expression is rejected by an rw str parameter';
    throws-like { takes-int(42) }, X::Parameter::RW, got => 42,
      'a boxed value without a container is rejected by an rw int parameter';
}

# vim: expandtab shiftwidth=4
