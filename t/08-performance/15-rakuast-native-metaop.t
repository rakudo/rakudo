use lib <t/packages/Test-Helpers>;
use Test::Helpers::QAST;
use Test;
use nqp;
plan 37;

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

# A native target takes the operator's result by assignment, with the
# target passed to the operator as the native it is, so the compound
# form agrees with its expansion on which candidate the operator
# dispatch lands on.
{
    multi sub infix:<+>(int $a, int $b) is default { 42 }
    my int $i = 1; $i += 1;
    is $i, 42, 'a native int compound assignment dispatches on the native target';
    my int $j = 1; $j = $j + 1;
    is $j, 42, 'the expanded form of a native int compound assignment agrees';
}
{
    multi sub infix:<~>(str $a, str $b) is default { 'nativeuser' }
    my str $s = 'a'; $s ~= 'b';
    is $s, 'nativeuser', 'a native str compound assignment dispatches on the native target';
}
{
    multi sub infix:<+>(num $a, num $b) is default { 42e0 }
    my num $n = 1e0; $n += 1e0;
    is $n, 42e0, 'a native num compound assignment dispatches on the native target';
}
{
    my class Counted { has int $!i = 1; method bump() { $!i += 1 } }
    multi sub infix:<+>(int $a, int $b) is default { 42 }
    is Counted.new.bump, 42, 'a native attribute compound assignment dispatches on the native target';
}
{
    multi sub infix:<+>(int $a, int $b) is default { 42 }
    sub bump(int $x is copy) { $x += 1; $x }
    is bump(1), 42, 'a native copy parameter compound assignment dispatches on the native target';
}
{
    sub bump(int $x is rw) { $x += 1 }
    my int $v = 1; bump($v);
    is $v, 2, 'a native rw parameter compound assignment writes through the reference';
}
{
    multi sub infix:<+>(int $a, int $b) is default { 42 }
    sub bump(int $x is rw) { $x += 1 }
    my int $v = 1; bump($v);
    is $v, 42, 'a native rw parameter compound assignment dispatches on the native it refers to';
}
dies-ok { sub bump(int $x) { $x += 1 }; bump(1) },
    'a read-only native parameter compound assignment reports the mutability';
{
    use soft;
    my $handle = &infix:<+>.wrap(-> |c { 100 });
    my int $i = 1; my $x = 1; $i += $x;
    $handle.restore;
    is $i, 100, 'a native compound assignment under the soft pragma runs the wrapped operator';
}
{
    my int $c = 1;
    my @l = ($c += 1), ($c += 1);
    is-deeply @l, [2, 3], 'a native compound assignment yields its value rather than the variable';
}
{
    my int $i = 1; my $x = 1; ($i) += $x;
    is $i, 2, 'a parenthesized native target compound-assigns through the metaop';
}
{
    my int $j += 5;
    is $j, 5, 'a native declaration as the target compound-assigns through the metaop';
}
throws-like { my int $i = 1; $i ^^= 1 }, Exception, message => /Nil/,
    'a native exclusive-or compound assignment reports the Nil it cannot store';

# An integer literal operand takes the operator call, which pairs the
# literal with the native target, so `$i += 1` wraps as `$i + 1` does.
{
    my int $i = 9223372036854775807;
    $i += 1;
    is $i, -9223372036854775808, 'native int += an integer literal wraps like its expanded form';
}

# The remaining cases are RakuAST-frontend specific: the frontend emits
# the native raw ops directly, where the legacy optimizer reaches the
# native ops a different way, so these are pinned to RakuAST.
if nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast' {
    qast-is 'my int $i; $i += 5', -> \v { not qast-contains-op(v, 'add_i') and qast-contains-call(v, '&infix:<+>') },
        'an integer literal keeps the operator call';
    qast-is 'my int $i; $i += 5', -> \v { qast-contains-op(v, 'assign_i') and not qast-contains-call(v, '&METAOP_ASSIGN') },
        'an integer literal operand assigns the operator result to the native target';
    qast-is 'my int $i; my $x; $i += $x', -> \v { qast-contains-op(v, 'assign_i') and not qast-contains-call(v, '&METAOP_ASSIGN') },
        'a boxed operand assigns the operator result to the native target';
    qast-is 'my uint $u; $u += 5', -> \v { qast-contains-op(v, 'assign_u') and not qast-contains-call(v, '&METAOP_ASSIGN') },
        'an unsigned native target assigns the operator result';
    qast-is 'my int $g; $g min= 3', -> \v { qast-contains-op(v, 'assign_i') and not qast-contains-call(v, '&METAOP_ASSIGN') },
        'a list associative operator assigns its result to the native target';
    qast-is 'sub f(int $x is rw) { my $y; $x += $y }', :full, -> \v { qast-contains-op(v, 'assign_i') and not qast-contains-call(v, '&METAOP_ASSIGN') },
        'an rw native parameter target assigns the operator result through its alias';
    qast-is 'my int $i; $i //= 5', -> \v { not qast-contains-op v, 'assign_i' },
        'a test operator on a native target keeps the metaop';
    qast-is 'my int $i; my int $n; $i += $n', -> \v { qast-contains-op v, 'add_i' },
        'native int operands lower to a raw op';
    qast-is 'my num $a; $a += 1.5e0', -> \v { qast-contains-op v, 'add_n' },
        'a native float literal lowers to a raw op';
    qast-is 'sub f(int $i is copy) { my int $n; $i += $n }', :full, -> \v { qast-contains-op v, 'add_i' },
        'a native copy parameter compound-steps to a raw op';
    qast-is 'sub f(num $x is copy) { $x += 1.5e0 }', :full, -> \v { qast-contains-op v, 'add_n' },
        'a num copy parameter with a float literal lowers to a raw op';
    qast-is 'sub f(int $j) { my int $t; $t += $j }', :full, -> \v { qast-contains-op v, 'add_i' },
        'a native parameter read on the right lowers to a raw op';
    qast-is 'sub f(int $j is rw) { my int $t; $t += $j }', :full, -> \v { not qast-contains-op v, 'add_i' },
        'an rw parameter on the right keeps the metaop call';
    qast-is 'class C { has int $!a; method m { my int $t; $t += $!a } }', :full, -> \v { qast-contains-op v, 'add_i' },
        'a native attribute read on the right lowers to a raw op';
}
else {
    skip 'integer-literal and native lowering shape is RakuAST-specific', 14;
}

# vim: expandtab shiftwidth=4
