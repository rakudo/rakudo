class Rat {
    has $.numerator;
    has $.denominator;

    sub gcd(Int $a is copy, Int $b is copy) {
        $a = -$a if ($a < 0);
        $b = -$b if ($b < 0);
        while $a > 0 && $b > 0 {
            # ($a, $b) = ($b, $a) if ($b > $a);  # TODO: next block could be this if NG supported it
            if ($b > $a)
            {
                my $temp = $a;
                $a = $b;
                $b = $temp;
            }
            $a %= $b;
        }
        return $a + $b;
    }

    multi method new(Int $numerator is copy, Int $denominator is copy) {
        if $denominator < 0 {
            $numerator = -$numerator;
            $denominator = -$denominator;
        }
        my $gcd = gcd($numerator, $denominator);
        $numerator = $numerator div $gcd;
        $denominator = $denominator div $gcd;
        self.bless(*, :numerator($numerator), :denominator($denominator));
    }

    multi method perl() { "$!numerator/$!denominator"; }

    our Bool multi method Bool() { $!numerator != 0 ?? Bool::True !! Bool::False }

    multi method Num() { $!numerator.Num / $!denominator.Num; }

    multi method Rat() { self; }

    multi method Int() { self.Num.Int; }

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

    multi method abs() {
        self < 0 ?? -self !! self;
    }

    our Int multi method sign {
        # self ~~ NaN ?? NaN !! self <=> 0;
        self < 0 ?? -1 !! ( self == 0 ?? 0 !! 1);
    }
}

multi sub infix:<+>(Rat $a, Rat $b) {
    my $gcd = Rat::gcd($a.denominator, $b.denominator);
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
    my $gcd = Rat::gcd($a.denominator, $b.denominator);
    ($a.numerator * ($b.denominator div $gcd) - $b.numerator * ($a.denominator div $gcd))
        / (($a.denominator div $gcd) * $b.denominator);
}

multi sub infix:<->(Rat $a, Int $b) {
    ($a.numerator - $b * $a.denominator) / $a.denominator;
}

multi sub infix:<->(Int $a, Rat $b) {
    ($a * $b.denominator - $b.numerator) / $b.denominator;
}

multi sub prefix:<->(Rat $a) {
    Rat.new(-$a.numerator, $a.denominator);
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

augment class Int {
    # CHEAT: Comes from Int.pm, moved here for the moment.
    our Rat multi method Rat() { Rat.new(self, 1); }
}

# vim: ft=perl6 sw=4 ts=4 expandtab
