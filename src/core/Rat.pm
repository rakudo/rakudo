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

    multi method Num() { $!numerator.Num / $!denominator.Num; }

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

# CHEAT: In the subs that follow, all the seemingly unneeded calls to .Int
# are to get around an early rakudo ng bug.  Once that bug is fixed,
# it is advised that this patch be backed out to get rid of the .Ints.

our multi sub infix:<+>(Rat $a, Rat $b) {
    Rat.new(($a.numerator * $b.denominator + $b.numerator * $a.denominator).Int,
            ($a.denominator * $b.denominator).Int );
}

our multi sub infix:<+>(Rat $a, Int $b) {
    Rat.new(($a.numerator + $b * $a.denominator).Int, $a.denominator);
}

our multi sub infix:<+>(Int $a, Rat $b) {
    Rat.new(($a * $b.denominator + $b.numerator).Int, $b.denominator);
}

our multi sub infix:<->(Rat $a, Rat $b) {
    Rat.new(($a.numerator * $b.denominator - $b.numerator * $a.denominator).Int,
            ($a.denominator * $b.denominator).Int );
}

our multi sub infix:<->(Rat $a, Int $b) {
    Rat.new(($a.numerator - $b * $a.denominator).Int, $a.denominator);
}

our multi sub infix:<->(Int $a, Rat $b) {
    Rat.new(($a * $b.denominator - $b.numerator).Int, $b.denominator);
}

our multi sub prefix:<->(Rat $a) {
    Rat.new((-$a.numerator).Int, $a.denominator);
}

our multi sub infix:<*>(Rat $a, Rat $b) {
    Rat.new(($a.numerator * $b.numerator).Int, ($a.denominator * $b.denominator).Int);
}

our multi sub infix:<*>(Rat $a, Int $b) {
    Rat.new(($a.numerator * $b).Int, $a.denominator);
}

our multi sub infix:<*>(Int $a, Rat $b) {
    Rat.new(($a * $b.numerator).Int, $b.denominator);
}

our multi sub infix:</>(Rat $a, Rat $b) {
    Rat.new(($a.numerator * $b.denominator).Int, ($a.denominator * $b.numerator).Int);
}

our multi sub infix:</>(Rat $a, Int $b) {
    Rat.new($a.numerator, ($a.denominator * $b).Int);
}

our multi sub infix:</>(Int $a, Rat $b) {
    Rat.new(($b.denominator * $a).Int, $b.numerator);
}

our multi sub infix:</>(Int $a, Int $b) {
    Rat.new($a, $b);
}

augment class Int {
    # CHEAT: Comes from Int.pm, moved here for the moment.
    our Rat multi method Rat() { Rat.new(self, 1); }
}

# vim: ft=perl6 sw=4 ts=4 expandtab
