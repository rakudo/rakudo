my Int $UINT64_UPPER = nqp::pow_I(2, 64, Num, Int);

my role Rational is Real {
    has Int $.numerator;
    has Int $.denominator;

    multi method WHICH(Rat:D:) {
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
          !!  $!numerator.Num / $!denominator.Num
    }
    method Int() { $!numerator div $!denominator }

    method Bridge() { self.Num }
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

my class FatRat { ... }
# XXX: should also be Cool
my class Rat    does Rational { 
    method Rat   (Rat:D: Real $?) { self }
    method FatRat(Rat:D: Real $?) { FatRat.new($.numerator, $.denominator); }
}
my class FatRat does Rational {
    method FatRat(FatRat:D: Real $?) { self }
    method Rat   (FatRat:D: Real $?) {
        $.denominator < $UINT64_UPPER
            ?? Rat.new($.numerator, $.denominator)
            !! fail "Cannot convert from FatRat to Rat because denominator is too big";
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
    if nqp::istype($t1, FatRat) || nqp::istype($t1, FatRat) {
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
        # TODO: be smarter here if both integers are big
        nqp::p6box_n(nqp::div_n(
                nqp::tonum_I(nqp::p6decont $numerator),
                nqp::tonum_I(nqp::p6decont $denominator)
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
    $a.numerator / ($a.denominator * $b);
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

