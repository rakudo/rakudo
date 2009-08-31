class Rat {
    has $.numerator;
    has $.denominator;

    multi method new(Int $numerator is copy, Int $denominator is copy) {
        if $denominator < 0 {
            $numerator = -$numerator;
            $denominator = -$denominator;
        }
        self.bless(*, :$numerator, :$denominator);
    }

    multi method Str() { "$!numerator/$!denominator"; }

    multi method Num() { $!numerator + 0.0 / $!denominator }
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
    Rat.new($a.numerator * $b.denominator + $b.numerator * $a.denominator,
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
