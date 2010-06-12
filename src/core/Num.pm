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

    method Int() {
        Q:PIR {
            $P0 = find_lex 'self'
            $I0 = $P0
            $P1 = new ['Int']
            $P1 = $I0
            %r  = $P1
        }
    }

    method Rat(Real $epsilon = 1.0e-6) {
        my sub modf($num) { my $q = $num.Int; $num - $q, $q; }

        my $num = +self;
        my $signum = $num < 0 ?? -1 !! 1;
        $num = -$num if $signum == -1;

        # Find convergents of the continued fraction.

        my ($r, $q) = modf($num);
        my ($a, $b) = 1, $q;
        my ($c, $d) = 0, 1;

        while $r != 0 && abs($num - ($b/$d)) > $epsilon {
            ($r, $q) = modf(1/$r);

            ($a, $b) = ($b, $q*$b + $a);
            ($c, $d) = ($d, $q*$d + $c);
        }

        # Note that this result has less error than any Rational with a
        # smaller denominator but it is not (necessarily) the Rational
        # with the smallest denominator that has less than $epsilon error.
        # However, to find that Rational would take more processing.

        ($signum * $b) / $d;
    }

    method Num() { self; }

    method ln(Num $x:) {
        pir::ln__Nn($x);
    }

    multi method perl() {
        ~self;
    }

    method sqrt(Num $x:) {
        pir::sqrt__Nn($x);
    }

    method floor(Real $x:) {
        given $x {
            when NaN { NaN }
            when Inf { Inf }
            when -Inf { -Inf }
            pir::box__PI(pir::floor__IN($x));
        }
    }

    method ceiling(Num $x:) {
        given $x {
            when NaN { NaN }
            when Inf { Inf }
            when -Inf { -Inf }
            pir::box__PI(pir::ceil__IN($x));
        }
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

multi sub infix:«<=>»(Num $a, Num $b) {
    # TODO: should be Order::Same, ::Increase, ::Decrease once they work
    if $a == $b {
        0;
    } else {
        $a < $b ?? -1 !! 1;
    }
}

multi sub infix:«==»(Num $a, Num $b) {
    pir::iseq__INN( $a, $b) ?? True !! False
}

multi sub infix:«!=»(Num $a, Num $b) {
    pir::iseq__INN( $a, $b) ?? False !! True # note reversed
}

multi sub infix:«<»(Num $a, Num $b) {
    pir::islt__INN( $a, $b) ?? True !! False
}

multi sub infix:«>»(Num $a, Num $b) {
    pir::isgt__INN( $a, $b) ?? True !! False
}

multi sub infix:«<=»(Num $a, Num $b) {
    pir::isgt__INN( $a, $b) ?? False !! True # note reversed
}

multi sub infix:«>=»(Num $a, Num $b) {
    pir::islt__INN( $a, $b) ?? False !! True # note reversed
}

# Arithmetic operators

multi sub prefix:<->(Num $a) {
    pir::neg__NN($a);
}

multi sub infix:<+>(Num $a, Num $b) {
    pir::add__NNN($a, $b)
}

multi sub infix:<->(Num $a, Num $b) {
    pir::sub__NNN($a, $b)
}

multi sub infix:<*>(Num $a, Num $b) {
    pir::mul__NNN($a, $b)
}

multi sub infix:</>(Num $a, Num $b) {
    pir::div__NNN($a, $b)
}

multi sub infix:<**>(Num $a, Num $b) {
    pir::pow__NNN($a, $b)
}


# vim: ft=perl6
