use Test;

# A rw parameter of native type binds the caller's native reference, so
# an assignment to the parameter writes back through it. The rw-ness can
# come from an is rw trait or from the <-> block starter. In a <-> block
# an is copy trait keeps the parameter rw, since the block starter sets
# the rw flag on every parameter.

plan 16;

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

# The frontends throw different exception types here, so the message
# carries the assertion.
throws-like { for 1..3 <-> int $i { } },
  Exception,
  message => /'modifiable native int'/,
  'a for loop over a range of values rejects a <-> int parameter';

# vim: expandtab shiftwidth=4
