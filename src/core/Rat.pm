class Rat is Cool does Real {
    has $.numerator;
    has $.denominator;


    multi method new() {
        self.bless(*, :numerator(0), :denominator(1));

    }

    multi method new(Int $numerator is copy, Int $denominator is copy) {
        if $denominator < 0 {
            $numerator = -$numerator;
            $denominator = -$denominator;
        }
        my $gcd = pir::gcd__iii($numerator, $denominator);
        $numerator = $numerator div $gcd;
        $denominator = $denominator div $gcd;
        self.bless(*, :numerator($numerator), :denominator($denominator));
    }

    multi method nude() { $.numerator, $.denominator; }

    multi method perl() { "$!numerator/$!denominator"; }

    method Bridge() {
        $!denominator == 0 ?? Inf * $!numerator.sign
                           !! $!numerator.Bridge / $!denominator.Bridge;
    }

    method Bool() { $!numerator != 0 ?? Bool::True !! Bool::False }

    method Rat(Real $epsilon = 1.0e-6) { self; }

    method Num() {
        $!denominator == 0 ?? Inf * $!numerator.sign
                           !! $!numerator.Num / $!denominator.Num;
    }

    method succ {
        Rat.new($!numerator + $!denominator, $!denominator);
    }

    method pred {
        Rat.new($!numerator - $!denominator, $!denominator);
    }
}

multi sub prefix:<->(Rat $a) {
    Rat.new(-$a.numerator, $a.denominator);
}

multi sub infix:<+>(Rat $a, Rat $b) {
    my $gcd = pir::gcd__iii($a.denominator, $b.denominator);
    ($a.numerator * ($b.denominator div $gcd) + $b.numerator * ($a.denominator div $gcd))
        / (($a.denominator div $gcd) * $b.denominator);
}

multi sub infix:<+>(Rat $a, Int $b) {
    ($a.numerator + $b * $a.denominator) / $a.denominator;
}

multi sub infix:<+>(Int $a, Rat $b) {
    ($a * $b.denominator + $b.numerator) / $b.denominator;
}

multi sub infix:<->(Rat $a, Rat $b) {
    my $gcd = pir::gcd__iii($a.denominator, $b.denominator);
    ($a.numerator * ($b.denominator div $gcd) - $b.numerator * ($a.denominator div $gcd))
        / (($a.denominator div $gcd) * $b.denominator);
}

multi sub infix:<->(Rat $a, Int $b) {
    ($a.numerator - $b * $a.denominator) / $a.denominator;
}

multi sub infix:<->(Int $a, Rat $b) {
    ($a * $b.denominator - $b.numerator) / $b.denominator;
}

multi sub infix:<*>(Rat $a, Rat $b) {
    ($a.numerator * $b.numerator) / ($a.denominator * $b.denominator);
}

multi sub infix:<*>(Rat $a, Int $b) {
    ($a.numerator * $b) / $a.denominator;
}

multi sub infix:<*>(Int $a, Rat $b) {
    ($a * $b.numerator) / $b.denominator;
}

multi sub infix:</>(Rat $a, Rat $b) {
    ($a.numerator * $b.denominator) / ($a.denominator * $b.numerator);
}

multi sub infix:</>(Rat $a, Int $b) {
    $a.numerator / ($a.denominator * $b);
}

multi sub infix:</>(Int $a, Rat $b) {
    ($b.denominator * $a) / $b.numerator;
}

multi sub infix:</>(Int $a, Int $b) {
    Rat.new($a, $b);
}

# vim: ft=perl6 sw=4 ts=4 expandtab
