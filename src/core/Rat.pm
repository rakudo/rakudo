my Int $UINT64_UPPER = nqp::pow_I(2, 64, Num, Int);

my role Rational is Real {
    has Int $.numerator;
    has Int $.denominator;

    multi method WHICH(Rational:D:) {
        nqp::box_s(
            nqp::concat_s(
                nqp::concat_s(nqp::unbox_s(self.^name), '|'),
                nqp::concat_s(
                    nqp::tostr_I($!numerator),
                    nqp::concat_s('/', nqp::tostr_I($!denominator))
                )
            ),
            ObjAt
        );
    }

    method new(Int \$nu = 0, Int \$de = 1) {
        my $new         := nqp::create(self);
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
          !! nqp::p6box_n(nqp::div_In(
                nqp::p6decont($!numerator),
                nqp::p6decont($!denominator)
             ));
    }
    method Int() { $!numerator div $!denominator }

    method Bridge() { self.Num }
    multi method Str(Rational:D:) {
        self.Num.Str
    }
    method succ {
        self.new($!numerator + $!denominator, $!denominator);
    }

    method pred {
        self.new($!numerator - $!denominator, $!denominator);
    }
}

# XXX: should also be Cool
my class Rat    does Rational { 
    method Rat   (Rat:D: Real $?) { self }
    method FatRat(Rat:D: Real $?) { FatRat.new($.numerator, $.denominator); }
    multi method perl(Rat:D:) {
        $.numerator ~ '/' ~ $.denominator
    }
}
my class FatRat does Rational {
    method FatRat(FatRat:D: Real $?) { self }
    method Rat   (FatRat:D: Real $?) {
        $.denominator < $UINT64_UPPER
            ?? Rat.new($.numerator, $.denominator)
            !! fail "Cannot convert from FatRat to Rat because denominator is too big";
    }
    multi method perl(FatRat:D:) {
        "FatRat.new($.numerator, $.denominator)";
    }
}

sub DIVIDE_NUMBERS(Int:D \$nu, Int:D \$de, $t1, $t2) {
    my Int $gcd        := $nu gcd $de;
    my Int $numerator   = $nu div $gcd;
    my Int $denominator = $de div $gcd;
    if $denominator < 0 {
        $numerator   = -$numerator;
        $denominator = -$denominator;
    }
    if nqp::istype($t1, FatRat) || nqp::istype($t2, FatRat) {
        my $r := nqp::create(FatRat);
        nqp::bindattr($r, FatRat, '$!numerator',   nqp::p6decont($numerator));
        nqp::bindattr($r, FatRat, '$!denominator', nqp::p6decont($denominator));
        $r;
    } elsif $denominator < $UINT64_UPPER {
        my $r := nqp::create(Rat);
        nqp::bindattr($r, Rat, '$!numerator',   nqp::p6decont($numerator));
        nqp::bindattr($r, Rat, '$!denominator', nqp::p6decont($denominator));
        $r;
    } else {
        nqp::p6box_n(nqp::div_In(
                nqp::p6decont($numerator),
                nqp::p6decont($denominator)
            )
        );
    }
}

multi prefix:<->(Rat \$a) {
    Rat.new(-$a.numerator, $a.denominator);
}
multi prefix:<->(FatRat \$a) {
    FatRat.new(-$a.numerator, $a.denominator);
}

multi infix:<+>(Rational \$a, Rational \$b) {
    my Int $gcd := $a.denominator gcd $b.denominator;
    DIVIDE_NUMBERS(
        ($a.numerator * ($b.denominator div $gcd) + $b.numerator * ($a.denominator div $gcd)),
        (($a.denominator div $gcd) * $b.denominator),
        $a,
        $b,
    );
}
multi sub infix:<+>(Rational \$a, Int \$b) {
    DIVIDE_NUMBERS(
        ($a.numerator + $b * $a.denominator),
        $a.denominator,
        $a,
        $b,
    );
}
multi sub infix:<+>(Int \$a, Rational \$b) {
    DIVIDE_NUMBERS(
        ($a * $b.denominator + $b.numerator),
        $b.denominator,
        $a,
        $b,
    );
}

multi sub infix:<->(Rational \$a, Rational \$b) {
    my Int $gcd = $a.denominator gcd $b.denominator;
    DIVIDE_NUMBERS
        $a.numerator * ($b.denominator div $gcd) - $b.numerator * ($a.denominator div $gcd),
        ($a.denominator div $gcd) * $b.denominator,
        $a,
        $b;
}

multi sub infix:<->(Rational \$a, Int \$b) {
    DIVIDE_NUMBERS
        $a.numerator - $b * $a.denominator,
        $a.denominator,
        $a,
        $b;
}

multi sub infix:<->(Int \$a, Rational \$b) {
    DIVIDE_NUMBERS
        $a * $b.denominator - $b.numerator,
        $b.denominator,
        $a,
        $b;
}

multi sub infix:<*>(Rational \$a, Rational \$b) {
    DIVIDE_NUMBERS
        $a.numerator * $b.numerator,
        $a.denominator * $b.denominator,
        $a,
        $b;
}

multi sub infix:<*>(Rational \$a, Int \$b) {
    DIVIDE_NUMBERS
        $a.numerator * $b,
        $a.denominator,
        $a,
        $b;
}

multi sub infix:<*>(Int \$a, Rational \$b) {
    DIVIDE_NUMBERS
        $a * $b.numerator,
        $b.denominator,
        $a,
        $b;
}

multi sub infix:</>(Rational \$a, Rational \$b) {
    DIVIDE_NUMBERS
        $a.numerator * $b.denominator,
        $a.denominator * $b.numerator,
        $a,
        $b;
}

multi sub infix:</>(Rational \$a, Int \$b) {
    DIVIDE_NUMBERS
        $a.numerator,
        $a.denominator * $b,
        $a,
        $b;
}

multi sub infix:</>(Int \$a, Rational \$b) {
    DIVIDE_NUMBERS
        $b.denominator * $a,
        $b.numerator,
        $a,
        $b;
}

multi sub infix:</>(Int \$a, Int \$b) {
    DIVIDE_NUMBERS $a, $b, $a, $b
}

multi sub infix:<**>(Rational \$a, Int \$b) {
    DIVIDE_NUMBERS
        $a.numerator ** $b,
        $a.denominator ** $b,
        $a,
        $b;
}

multi sub infix:<==>(Rational:D \$a, Rational:D \$b) {
    $a.numerator == $b.numerator && $a.denominator == $b.denominator
}
multi sub infix:<==>(Rational:D \$a, Int:D \$b) {
    $a.numerator == $b && $a.denominator == 1
}
multi sub infix:<==>(Int:D \$a, Rational:D \$b) {
    $a == $b.numerator && $b.denominator == 1;
}

multi sub infix:«<»(Rational:D \$a, Rational:D \$b) {
    $a.numerator * $b.denominator < $b.numerator * $a.denominator
}
multi sub infix:«<»(Rational:D \$a, Int:D \$b) {
    $a.numerator  < $b * $a.denominator
}
multi sub infix:«<»(Int:D \$a, Rational:D \$b) {
    $a * $b.denominator < $b.numerator * $a
}

multi sub infix:«<=»(Rational:D \$a, Rational:D \$b) {
    $a.numerator * $b.denominator <= $b.numerator * $a.denominator
}
multi sub infix:«<=»(Rational:D \$a, Int:D \$b) {
    $a.numerator  <= $b * $a.denominator
}
multi sub infix:«<=»(Int:D \$a, Rational:D \$b) {
    $a * $b.denominator <= $b.numerator * $a
}

multi sub infix:«>»(Rational:D \$a, Rational:D \$b) {
    $a.numerator * $b.denominator > $b.numerator * $a.denominator
}
multi sub infix:«>»(Rational:D \$a, Int:D \$b) {
    $a.numerator  > $b * $a.denominator
}
multi sub infix:«>»(Int:D \$a, Rational:D \$b) {
    $a * $b.denominator > $b.numerator * $a
}

multi sub infix:«>=»(Rational:D \$a, Rational:D \$b) {
    $a.numerator * $b.denominator >= $b.numerator * $a.denominator
}
multi sub infix:«>=»(Rational:D \$a, Int:D \$b) {
    $a.numerator  >= $b * $a.denominator
}
multi sub infix:«>=»(Int:D \$a, Rational:D \$b) {
    $a * $b.denominator >= $b.numerator * $a
}

multi sub infix:«<=>»(Rational:D \$a, Rational:D \$b) {
    $a.numerator * $b.denominator <=> $b.numerator * $a.denominator
}
multi sub infix:«<=>»(Rational:D \$a, Int:D \$b) {
    $a.numerator  <=> $b * $a.denominator
}
multi sub infix:«<=>»(Int:D \$a, Rational:D \$b) {
    $a * $b.denominator <=> $b.numerator
}
