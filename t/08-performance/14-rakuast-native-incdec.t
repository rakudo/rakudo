use lib <t/packages/Test-Helpers>;
use Test::Helpers::QAST;
use Test;
plan 12;

# A native int or num `++`/`--` lowers to a raw op instead of calling the
# operator. Postfix returns the original value, prefix the stepped one.
{
    my int $i = 5; $i++;
    is $i, 6, 'native int increment steps the value';
}
{
    my int $i = 5; my int $r = $i++;
    is "$r $i", "5 6", 'native int post-increment returns the original';
}
{
    my int $i = 5; $i--;
    is $i, 4, 'native int decrement steps the value';
}
{
    my num $n = 2e0; $n++;
    is $n, 3e0, 'native num increment steps the value';
}
{
    my int $i = 5; my int $r = ++$i;
    is "$r $i", "6 6", 'native int pre-increment returns the stepped value';
}
{
    my int $i = 5; --$i;
    is $i, 4, 'native int pre-decrement steps the value';
}

# A prefix returns the stepped value directly, so unlike a postfix it lowers at
# a narrow width too, where the assignment truncates correctly.
{
    my int8 $a = 127; my $r = ++$a;
    is "$r $a", "-128 -128", 'a narrow native pre-increment wraps correctly';
}

{
    sub postfix:<++>(\x) { 999 }
    my int $i = 5;
    is $i++, 999, 'a user-redefined postfix is not lowered';
}
{
    sub prefix:<++>(\x) { 999 }
    my int $i = 5;
    is ++$i, 999, 'a user-redefined prefix is not lowered';
}

# Only a native operand lowers. These observe the emitted QAST.
qast-is 'my int $i; $i++', -> \v { qast-contains-op v, 'add_i' },
    'a native postfix operand lowers to a raw op';
qast-is 'my int $i; ++$i', -> \v { qast-contains-op v, 'add_i' },
    'a native prefix operand lowers to a raw op';
qast-is 'my Int $i = 0; $i++', -> \v { not qast-contains-op v, 'add_i' },
    'a boxed operand keeps the operator call';

# vim: expandtab shiftwidth=4
