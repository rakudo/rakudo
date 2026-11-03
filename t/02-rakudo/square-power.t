use Test;
plan 29;

{
    my $x = 7;
    is $x ** 2, 49, 'an Int square computes the power';
}
{
    my $x = -3;
    is $x ** 2, 9, 'a negative base squares to a positive';
}
{
    my $x = 2 ** 100;
    is $x ** 2, 2 ** 200, 'a big Int square computes the power';
}
{
    my $x = 7;
    is $x², 49, 'a superscript square computes the power';
}
{
    my constant $K = 6;
    is $K ** 2, 36, 'a sigiled constant square computes the power';
}
{
    my $r = 3/2;
    is-deeply $r ** 2, 9/4, 'a Rat square computes the power';
}
{
    my $r = FatRat.new(3, 2);
    is-deeply $r ** 2, FatRat.new(9, 4), 'a FatRat square stays a FatRat';
}
{
    my $r = 1 / 2 ** 40;
    is-deeply $r ** 2, 8.271806125530277e-25,
        'a Rat square whose denominator outgrows a Rat becomes a Num';
}

# Some libm pow implementations are off in the last place for this base.
{
    my $x = -3.263219580159655e-56;
    my $product = $x * $x;
    my @a = $x;
    my num $n = $x;
    my num $two = 2e0;
    is-deeply $x ** 2, $product, 'a Num square is the product of the base with itself';
    is-deeply @a[0] ** 2, $product, 'a Num element square is the product of the base with itself';
    is-deeply $x ** 2e0, $product, 'a Num square by a Num exponent is the product of the base with itself';
    is-deeply $x², $product, 'a superscript Num square is the product of the base with itself';
    my $y = $x;
    $y **= 2;
    is-deeply $y, $product, 'a Num square by power assignment is the product of the base with itself';
    is-deeply $n ** 2, $product, 'a native num square is the product of the base with itself';
    is-deeply $n ** 2e0, $product,
        'a native num square by a native exponent is the product of the base with itself';
    is-deeply $n ** $two, $product,
        'a native num square by a native exponent variable is the product of the base with itself';
}
{
    my $x = 0e0;
    my num $n = 0e0;
    is-deeply $x ** 2, 0e0, 'a zero Num square is zero';
    is-deeply $n ** 2e0, 0e0, 'a native num zero square by a native exponent is zero';
}
{
    my $x = 1e200;
    is-deeply $x ** 2, Inf, 'a Num square that overflows is infinite';
}
{
    my @a = 1e-200;
    fails-like { @a[0] ** 2 }, X::Numeric::Underflow, 'a Num square that underflows fails';
}
{
    my num $x = 1e-200;
    fails-like { $x ** 2e0 }, X::Numeric::Underflow, 'a native num square that underflows fails';
}
{
    my $x = 2e0;
    is-deeply $x ** 3, 8e0, 'a Num cube computes the power';
    is-deeply $x ** -1, 0.5e0, 'a Num to a negative Int computes the power';
    my $h = 0.5e0;
    my $huge = 2 ** 2000;
    is-deeply $h ** $huge, 0e0, 'a Num to an Int beyond the Num range reaches the limit';
    my @t = 1e-200;
    fails-like { @t[0] ** 3 }, X::Numeric::Underflow, 'a Num cube that underflows fails';
}
{
    my class Bridged is Num { method Bridge { 99e0 } }
    is Bridged.new(1.5e0) ** 2, 9801e0, 'a Num subclass square bridges the base';
    is Bridged.new(1.5e0) ** 3, 970299e0, 'a Num to an Int exponent other than 2 bridges the base';
    my class BridgedInt is Int { method Bridge { 3e0 } }
    is 2e0 ** BridgedInt.new(2), 8e0, 'a Num square by an Int subclass exponent bridges the exponent';
    is 2e0 ** BridgedInt.new(5), 8e0, 'a Num to an Int subclass exponent other than 2 bridges the exponent';
}

# vim: expandtab shiftwidth=4
