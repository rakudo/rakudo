# XXX: should also be Cool, but attributes and MI don't seem to mix yet
my class Rat is Real {
    has $.numerator;
    has $.denominator;

    method new(Int \$nu = 0, Int \$de = 1) {
        my $new := nqp::create(self);
        my $gcd         = $nu gcd $de;
        my $numerator   = $nu div $gcd;
        my $denominator = $de div $gcd;
        if $denominator < 0 {
            $numerator   = -$numerator;
            $denominator = -$denominator;
        }
        $new.BUILD($numerator, $denominator);
        $new;
    }
    method BUILD(Int \$nu, Int \$de) {
        $!numerator   = $nu;
        $!denominator = $de;
    }

    method nude() { $!numerator, $!denominator }
    method Num() {
        $!denominator == 0
          ?? ($!numerator < 0 ?? -$Inf !! $Inf)
          !!  $!numerator.Num / $!denominator.Num
    }
    method Int() { self.Num.Int }

    method Bridge() { self.Num }
    method Rat(Rat:D: Real $?) { self }
    multi method Str(Rat:D:) {
        self.Num.Str
    }
    multi method perl(Rat:D:) {
        $!numerator ~ '/' ~ $!denominator
    }
    method succ {
        Rat.new($!numerator + $!denominator, $!denominator);
    }

    method pred {
        Rat.new($!numerator - $!denominator, $!denominator);
    }
}

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

