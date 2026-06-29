use lib <t/packages/Test-Helpers>;
use Test::Helpers::QAST;
use Test;
plan 12;

# A native int or num `+=`/`-=`/`*=` with a native operand lowers to a raw op
# instead of calling the metaop.
{
    my int $i = 5; my int $n = 3; $i += $n;
    is $i, 8, 'native int += steps by the operand';
}
{
    my int $i = 5; my int $n = 3; $i -= $n;
    is $i, 2, 'native int -= steps by the operand';
}
{
    my int $i = 5; my int $n = 3; $i *= $n;
    is $i, 15, 'native int *= steps by the operand';
}
{
    my num $a = 2e0; my num $b = 3e0; $a += $b;
    is $a, 5e0, 'native num += steps by the operand';
}

# A float literal lowers too, since a float never overflows to a bignum.
{
    my num $a = 2e0; $a += 1.5e0;
    is $a, 3.5e0, 'native num += a number literal';
}

# Two native operands step in machine width, so the result wraps on overflow.
{
    my int $i = 9223372036854775807; my int $n = 1; $i += $n;
    is $i, -9223372036854775808, 'an all-native step wraps on overflow';
}

{
    sub infix:<+>(\a, \b) { 999 }
    my int $i = 5; my int $n = 3; $i += $n;
    is $i, 999, 'a user-redefined operator is not lowered';
}

# A non-native operand never lowers, so no raw op on either frontend.
qast-is 'my $a = 0; my $b = 1; $a += $b', -> \v { not qast-contains-op v, 'add_i' },
    'a non-native operand keeps the metaop call';

# The remaining cases are RakuAST-frontend specific. The frontend leaves an
# integer literal to the metaop, so `$i += 1` overflows to a bignum and throws
# rather than wrapping, and it emits the native raw ops directly. The legacy
# optimizer instead lowers an int literal to a wrapping op and reaches the
# native ops a different way, so these are pinned to RakuAST.
if %*ENV<RAKUDO_RAKUAST> {
    my int $i = 9223372036854775807;
    dies-ok { $i += 1 }, 'native int += an integer literal throws on overflow';
    qast-is 'my int $i; $i += 5', -> \v { not qast-contains-op v, 'add_i' },
        'an integer literal keeps the metaop call';
    qast-is 'my int $i; my int $n; $i += $n', -> \v { qast-contains-op v, 'add_i' },
        'native int operands lower to a raw op';
    qast-is 'my num $a; $a += 1.5e0', -> \v { qast-contains-op v, 'add_n' },
        'a native float literal lowers to a raw op';
}
else {
    skip 'integer-literal and native lowering shape is RakuAST-specific', 4;
}

# vim: expandtab shiftwidth=4
