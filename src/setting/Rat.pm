class Rat {
    has $.numerator;
    has $.denominator;

    my sub gcd(Int $a is copy, Int $b is copy)
    {
        $a = -$a if ($a < 0);
        $b = -$b if ($b < 0);
        while $a > 0 && $b > 0
        {
            ($a, $b) = ($b, $a) if ($b > $a);
            $a -= $b;
        }
        return $a + $b;
    }

    multi method new(Int $numerator is copy, Int $denominator is copy) {
        if $denominator < 0 {
            $numerator = -$numerator;
            $denominator = -$denominator;
        }
        my $gcd = gcd($numerator, $denominator);
        $numerator /= $gcd;
        $denominator /= $gcd;
        self.bless(*, :$numerator, :$denominator);
    }

    multi method Str() { "$!numerator/$!denominator"; }

    multi method Num() { $!numerator.Num / $!denominator.Num }
}

multi sub infix:<+>(Rat $a, Rat $b) {
    Rat.new($a.numerator * $b.denominator + $b.numerator * $a.denominator,
            $a.denominator * $b.denominator );
}

multi sub infix:<+>(Rat $a, Int $b) {
    Rat.new($a.numerator + $b * $a.denominator, $a.denominator);
}

multi sub infix:<+>(Int $a, Rat $b) {
    Rat.new($a * $b.denominator + $b.numerator, $b.denominator);
}

multi sub infix:<->(Rat $a, Rat $b) {
    Rat.new($a.numerator * $b.denominator - $b.numerator * $a.denominator,
            $a.denominator * $b.denominator );
}

multi sub infix:<->(Rat $a, Int $b) {
    Rat.new($a.numerator - $b * $a.denominator, $a.denominator);
}

multi sub infix:<*>(Rat $a, Rat $b) {
    Rat.new($a.numerator * $b.numerator, $a.denominator * $b.denominator);
}

multi sub infix:</>(Rat $a, Rat $b) {
    Rat.new($a.numerator * $b.denominator, $a.denominator * $b.numerator);
}

multi sub infix:<div>(Int $a, Int $b) {
    Rat.new($a, $b);
}

# vim: ft=perl6 sw=4 ts=4 expandtab
