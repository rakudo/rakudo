use Test;

plan 5;

# A special variable ($_, $/, $!) bound inside a destructuring sub-signature is
# a real parameter, so the routine or block must not also declare an implicit
# one. Doing both declared the same lexical twice, which failed to compile.

{
    my @seen;
    for (a => 1, b => 2) -> (:key($k), :value($_)) {
        @seen.push("$k=$_");
    }
    is @seen.join(','), 'a=1,b=2',
        '$_ in a for-loop destructuring sub-signature binds the value';
}

{
    sub k(($_)) { $_ }
    is k((7,)), 7, '$_ in a positional sub-signature binds the destructured value';
}

{
    sub g(($/)) { $/ }
    is g((2,)), 2, '$/ in a sub-signature binds without a redeclaration';
}

{
    sub h(($!)) { $! }
    is h((1,)), 1, '$! in a sub-signature binds without a redeclaration';
}

{
    my @seen;
    for ((5,), (6,)) -> ($!) { @seen.push($!) }
    is @seen.join(','), '5,6', '$! in a for-loop sub-signature binds without a redeclaration';
}

# vim: expandtab shiftwidth=4
