class Complex { ... }

augment class Num does Real {
    multi method ACCEPTS($other) {
        if self eq 'NaN' {
            $other eq 'NaN';
        } else {
            $other == self;
        }
    }

    multi method ACCEPTS(Complex $other) {
        if self eq 'NaN' {
            $other.re eq 'NaN' || $other.im eq 'NaN';
        } else {
            $other.im == 0 && $other.re == self;
        }
    }

    method Bridge() {
        self;
    }

    multi method Bool() {
        self != 0.0e0
    }

    multi method Int() {
        Q:PIR {
            $P0 = find_lex 'self'
            $I0 = $P0
            $P1 = new ['Int']
            $P1 = $I0
            %r  = $P1
        }
    }

    multi method Num() { self; }

    multi method Complex() { self + 0i; }

    method !modf($num) { my $q = $num.Int; $num - $q, $q; }

    multi method Rat($epsilon = 1.0e-6) {
        my $num = +self;
        my $signum = $num < 0 ?? -1 !! 1;
        $num = -$num if $signum == -1;

        # Find convergents of the continued fraction.

        my ($r, $q) = self!modf($num);
        my ($a, $b) = 1, $q;
        my ($c, $d) = 0, 1;

        while $r != 0 && abs($num - ($b/$d)) > $epsilon {
            ($r, $q) = self!modf(1/$r);

            ($a, $b) = ($b, $q*$b + $a);
            ($c, $d) = ($d, $q*$d + $c);
        }

        # Note that this result has less error than any Rational with a
        # smaller denominator but it is not (necessarily) the Rational
        # with the smallest denominator that has less than $epsilon error.
        # However, to find that Rational would take more processing.

        ($signum * $b) / $d;
    }

    # multi method exp() {
    #     pir::exp__Nn(self);
    # }

    method ln(Num $x:) {
        pir::ln__Nn($x);
    }

    multi method perl() {
        ~self;
    }

    method ceiling(Num $x:) {
        given $x {
            when NaN { NaN }
            when Inf { Inf }
            when -Inf { -Inf }
            pir::box__PI(pir::ceil__IN($x));
        }
    }

    method floor(Real $x:) {
        given $x {
            when NaN { NaN }
            when Inf { Inf }
            when -Inf { -Inf }
            pir::box__PI(pir::floor__IN($x));
        }
    }

    method sqrt(Num $x:) {
        pir::sqrt__Nn($x);
    }

    method sin(Num $x: $base = Radians) {
        pir::sin__Nn($x.to-radians($base));
    }

    method asin(Num $x: $base = Radians) {
        pir::asin__Nn($x).from-radians($base);
    }

    method cos(Num $x: $base = Radians) {
        pir::cos__Nn($x.to-radians($base));
    }

    method acos(Num $x: $base = Radians) {
        pir::acos__Nn($x).from-radians($base);
    }

    method tan(Num $x: $base = Radians) {
        pir::tan__Nn($x.to-radians($base));
    }

    method atan(Num $x: $base = Radians) {
        pir::atan__Nn($x).from-radians($base);
    }

    method sec(Num $x: $base = Radians) {
        pir::sec__Nn($x.to-radians($base));
    }

    method asec(Num $x: $base = Radians) {
        pir::asec__Nn($x).from-radians($base);
    }

    method sinh(Num $x: $base = Radians) {
        pir::sinh__Nn($x.to-radians($base));
    }

    method asinh(Num $x: $base = Radians) {
        ($x + ($x * $x + 1).sqrt).log.from-radians($base);
    }

    method cosh(Num $x: $base = Radians) {
        pir::cosh__Nn($x.to-radians($base));
    }

    method acosh(Num $x: $base = Radians) {
        ($x + ($x * $x - 1).sqrt).log.from-radians($base);
    }

    method tanh(Num $x: $base = Radians) {
        pir::tanh__Nn($x.to-radians($base));
    }

    method atanh(Num $x: $base = Radians) {
        (((1 + $x) / (1 - $x)).log / 2).from-radians($base);
    }

    method sech(Num $x: $base = Radians) {
        pir::sech__Nn($x.to-radians($base));
    }

    method asech(Num $x: $base = Radians) {
        (1 / $x).acosh($base);
    }

    method cosech(Num $x: $base = Radians) {
        1 / $x.sinh($base);
    }

    method acosech(Num $x: $base = Radians) {
        (1 / $x).asinh($base);
    }

    method cosec(Num $x: $base = Radians) {
        1 / $x.sin($base);
    }

    method acosec(Num $x: $base = Radians) {
        (1 / $x).asin($base);
    }

    method cotan(Num $x: $base = Radians) {
        1 / $x.tan($base);
    }

    method acotan(Num $x: $base = Radians) {
        (1 / $x).atan($base);
    }

    method cotanh(Num $x: $base = Radians) {
        1 / $x.tanh($base);
    }

    method acotanh(Num $x: $base = Radians) {
        (1 / $x).atanh($base);
    }

    method atan2(Num $y: Num $x = 1, $base = Radians) {
        pir::atan__NNn($y, $x).from-radians($base);
    }
}

# vim: ft=perl6
