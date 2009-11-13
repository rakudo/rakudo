class Rat {
    has $.numerator;
    has $.denominator;

    sub gcd(Int $a is copy, Int $b is copy) {
        $a = -$a if ($a < 0);
        $b = -$b if ($b < 0);
        while $a > 0 && $b > 0 {
            ($a, $b) = ($b, $a) if ($b > $a);
            $a %= $b;
        }
        return $a + $b;
    }

    multi method new(Int $numerator is copy, Int $denominator is copy) {
        if $denominator < 0 {
            $numerator = -$numerator;
            $denominator = -$denominator;
        }
        #my $gcd = gcd($numerator, $denominator);
        #$numerator = $numerator div $gcd;
        #$denominator = $denominator div $gcd;
        self.bless(*, :numerator($numerator), :denominator($denominator));
    }

    multi method perl() { "$!numerator/$!denominator"; }

    multi method Num() { $!numerator.Num / $!denominator.Num }

    multi method Str() { $.Num.Str; }

    multi method nude() { $.numerator, $.denominator; }

    # Most of the trig functions for Rat are in Any-num.pm, but
    # sec is a special case.
    our Num multi method sec($base = 'radians') {
        self.Num.sec($base);
    }

    multi method succ {
        Rat.new($!numerator + $!denominator, $!denominator);
    }
    multi method pred {
        Rat.new($!numerator - $!denominator, $!denominator);
    }
}

our multi sub infix:<+>(Rat $a, Rat $b) {
    Rat.new($a.numerator * $b.denominator + $b.numerator * $a.denominator,
            $a.denominator * $b.denominator );
}

our multi sub infix:<+>(Rat $a, Int $b) {
    Rat.new($a.numerator + $b * $a.denominator, $a.denominator);
}

our multi sub infix:<+>(Int $a, Rat $b) {
    Rat.new($a * $b.denominator + $b.numerator, $b.denominator);
}

our multi sub infix:<->(Rat $a, Rat $b) {
    Rat.new($a.numerator * $b.denominator - $b.numerator * $a.denominator,
            $a.denominator * $b.denominator );
}

our multi sub infix:<->(Rat $a, Int $b) {
    Rat.new($a.numerator - $b * $a.denominator, $a.denominator);
}

our multi sub infix:<->(Int $a, Rat $b) {
    Rat.new($a * $b.denominator - $b.numerator, $b.denominator);
}

our multi sub prefix:<->(Rat $a) {
    Rat.new(-$a.numerator, $a.denominator);
}

our multi sub infix:<*>(Rat $a, Rat $b) {
    Rat.new($a.numerator * $b.numerator, $a.denominator * $b.denominator);
}

our multi sub infix:<*>(Rat $a, Int $b) {
    Rat.new($a.numerator * $b, $a.denominator);
}

our multi sub infix:<*>(Int $a, Rat $b) {
    Rat.new($a * $b.numerator, $b.denominator);
}

our multi sub infix:</>(Rat $a, Rat $b) {
    Rat.new($a.numerator * $b.denominator, $a.denominator * $b.numerator);
}

our multi sub infix:</>(Rat $a, Int $b) {
    Rat.new($a.numerator, $a.denominator * $b);
}

our multi sub infix:</>(Int $a, Rat $b) {
    Rat.new($b.denominator * $a, $b.numerator);
}

our multi sub infix:</>(Int $a, Int $b) {
    Rat.new($a, $b);
}

# vim: ft=perl6 sw=4 ts=4 expandtab
