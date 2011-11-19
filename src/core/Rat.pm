my class Rat does Rational { }
multi prefix:<->(Rat \$a) {
    Rat.new(-$a.numerator, $a.denominator);
}

multi infix:<+>(Rat \$a, Rat \$b) {
    my $gcd = $a.denominator gcd $b.denominator;
    ($a.numerator * ($b.denominator div $gcd) + $b.numerator * ($a.denominator div $gcd))
        / (($a.denominator div $gcd) * $b.denominator);
}
multi sub infix:<+>(Rat \$a, Int \$b) {
    ($a.numerator + $b * $a.denominator) / $a.denominator;
}
multi sub infix:<+>(Int \$a, Rat \$b) {
    ($a * $b.denominator + $b.numerator) / $b.denominator;
}

multi sub infix:<->(Rat \$a, Rat \$b) {
    my $gcd = $a.denominator gcd $b.denominator;
    ($a.numerator * ($b.denominator div $gcd) - $b.numerator * ($a.denominator div $gcd))
        / (($a.denominator div $gcd) * $b.denominator);
}

multi sub infix:<->(Rat \$a, Int \$b) {
    ($a.numerator - $b * $a.denominator) / $a.denominator;
}

multi sub infix:<->(Int \$a, Rat \$b) {
    ($a * $b.denominator - $b.numerator) / $b.denominator;
}

multi sub infix:<*>(Rat \$a, Rat \$b) {
    ($a.numerator * $b.numerator) / ($a.denominator * $b.denominator);
}

multi sub infix:<*>(Rat \$a, Int \$b) {
    ($a.numerator * $b) / $a.denominator;
}

multi sub infix:<*>(Int \$a, Rat \$b) {
    ($a * $b.numerator) / $b.denominator;
}

multi sub infix:</>(Rat \$a, Rat \$b) {
    ($a.numerator * $b.denominator) / ($a.denominator * $b.numerator);
}

multi sub infix:</>(Rat \$a, Int \$b) {
    $a.numerator / ($a.denominator * $b);
}

multi sub infix:</>(Int \$a, Rat \$b) {
    ($b.denominator * $a) / $b.numerator;
}

multi sub infix:</>(Int \$a, Int \$b) {
    Rat.new($a, $b);
}

multi sub infix:<**>(Rat \$a, Int \$b) {
    Rat.new($a.numerator ** $b,$a.denominator ** $b);
}

