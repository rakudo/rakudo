my role Rational is Real {
    has Int $.numerator;
    has Int $.denominator;

    method new(Int \$nu = 0, Int \$de = 1) {
        my $new     := nqp::create(self);
        my Int $gcd     := $nu gcd $de;
        my $numerator   = $nu div $gcd;
        my $denominator = $de div $gcd;
        if $denominator < 0 {
            $numerator   = -$numerator;
            $denominator = -$denominator;
        }
        nqp::bindattr($new, self.WHAT, '$!numerator',     nqp::p6decont($numerator));
        nqp::bindattr($new, self.WHAT, '$!denominator',   nqp::p6decont($denominator));
        $new;
    }

    method nude() { $!numerator, $!denominator }
    method Num() {
        $!denominator == 0
          ?? ($!numerator < 0 ?? -$Inf !! $Inf)
          !!  $!numerator.Num / $!denominator.Num
    }
    method Int() { $!numerator div $!denominator }

    method Bridge() { self.Num }
    method Rat(Rational:D: Real $?) { self }
    multi method Str(Rational:D:) {
        self.Num.Str
    }
    multi method perl(Rational:D:) {
        $!numerator ~ '/' ~ $!denominator
    }
    method succ {
        Rat.new($!numerator + $!denominator, $!denominator);
    }

    method pred {
        Rat.new($!numerator - $!denominator, $!denominator);
    }
}

