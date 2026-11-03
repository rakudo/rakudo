use Test;

plan 4;

# A type capture in a pointy block declares a fresh lexical inside that
# block, the way a routine declares its own. A block reusing a name from an
# enclosing routine must not rebind the outer capture.

# Reusing the enclosing routine's capture name in a loop block leaves the
# outer capture intact.
is do {
    sub f(::T $x) { for 'a', 'b' -> ::T { }; T.^name }
    f(42)
}, 'Int', 'capture in loop block does not leak into enclosing routine capture';

# A distinct capture name in the block does not disturb the outer one.
is do {
    sub g(::T $x) { for 'a', 'b' -> ::U { }; T.^name }
    g(42)
}, 'Int', 'distinct capture in loop block leaves enclosing capture intact';

# A block binds its own capture to the argument type.
is do {
    my $b = -> ::U { U.^name };
    $b('hello')
}, 'Str', 'pointy block binds its own type capture to the argument type';

# A nested block capture sees its own binding, not the outer one.
is do {
    sub h(::T $x) { (-> ::T { T.^name })(3.14) }
    h(42)
}, 'Rat', 'nested block capture binds to its own argument, shadowing outer';
