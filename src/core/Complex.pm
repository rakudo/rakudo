class Complex does Numeric is Cool {
    has $.re;
    has $.im;

    multi method new($re, $im) {
        self.bless(*, :re($re), :im($im));
    }

    multi method ACCEPTS(Complex $topic) {
        ($topic.re ~~ $.re) && ($topic.im ~~ $.im);
    }
    multi method ACCEPTS($topic) {
        ($topic.Num ~~ $.re) && ($.im == 0);
    }

    multi method Complex() { self }

    method reals() {
        (self.re, self.im);
    }

    our Bool multi method Bool() { ( $!re != 0 || $!im != 0 ) ?? Bool::True !! Bool::False }

    multi method perl() {
        "Complex.new($.re, $.im)";
    }

    multi method Str() {
        "$.re + {$.im}i";
    }

    method abs(Complex $x:) {
        ($x.re * $x.re + $x.im * $x.im).sqrt
    }

    multi method exp() {
        Complex.new($.re.Num.exp * $.im.Num.cos, $.re.Num.exp * $.im.Num.sin);
    }

    multi method exp(Complex $exponent: Numeric $base) {
        $base ** $exponent;
    }

    method ln() {
        Q:PIR {
            .local pmc self
            self = find_lex 'self'
            $P0 = get_root_namespace ['parrot'; 'Complex' ]
            $P0 = get_class $P0
            $P0 = $P0.'new'()
            $N0 = self.'re'()
            $P0[0] = $N0
            $N1 = self.'im'()
            $P0[1] = $N1
            $P0 = $P0.'ln'()
            $N0 = $P0[0]
            $P2 = box $N0
            $N1 = $P0[1]
            $P3 = box $N1
            $P1 = get_hll_global 'Complex'
            $P1 = $P1.'new'($P2, $P3)
            %r  = $P1
        }
    }

    method sin(Complex $x: $base = Radians) {
        $x.re.sin($base) * $x.im.cosh($base) + ($x.re.cos($base) * $x.im.sinh($base))i;
    }

    method asin(Complex $x: $base = Radians) {
        (-1i * log(($x)i + sqrt(1 - $x * $x))).from-radians($base);
    }

    method cos(Complex $x: $base = Radians) {
        $x.re.cos($base) * $x.im.cosh($base) - ($x.re.sin($base) * $x.im.sinh($base))i;
    }

    method acos(Complex $x: $base = Radians) {
      (pi / 2).from-radians($base) - $x.asin($base);
    }

    method tan(Complex $x: $base = Radians) {
        $x.sin($base) / $x.cos($base);
    }

    method atan(Complex $x: $base = Radians) {
       ((log(1 - ($x)i) - log(1 + ($x)i))i / 2).from-radians($base);
    }

    method sec(Complex $x: $base = Radians) {
        1 / $x.cos($base);
    }

    method asec(Complex $x: $base = Radians) {
        (1 / $x).acos($base);
    }

    method cosec(Complex $x: $base = Radians) {
        1 / $x.sin($base);
    }

    method acosec(Complex $x: $base = Radians) {
        (1 / $x).asin($base);
    }

    method cotan(Complex $x: $base = Radians) {
        $x.cos($base) / $x.sin($base);
    }

    method acotan(Complex $x: $base = Radians) {
        (1 / $x).atan($base);
    }

    method sinh(Complex $x: $base = Radians) {
        -((1i * $x).sin($base))i;
    }

    method asinh(Complex $x: $base = Radians) {
       ($x + sqrt(1 + $x * $x)).log.from-radians($base);
    }

    method cosh(Complex $x: $base = Radians) {
        (1i * $x).cos($base);
    }

    method acosh(Complex $x: $base = Radians) {
       ($x + sqrt($x * $x - 1)).log.from-radians($base);
    }

    method tanh(Complex $x: $base = Radians) {
        -((1i * $x).tan($base))i;
    }

    method atanh(Complex $x: $base = Radians) {
       (((1 + $x) / (1 - $x)).log / 2).from-radians($base);
    }

    method sech(Complex $x: $base = Radians) {
        1 / $x.cosh($base);
    }

    method asech(Complex $x: $base = Radians) {
        (1 / $x).acosh($base);
    }

    method cosech(Complex $x: $base = Radians) {
        1 / $x.sinh($base);
    }

    method acosech(Complex $x: $base = Radians) {
        (1 / $x).asinh($base);
    }

    method cotanh(Complex $x: $base = Radians) {
        1 / $x.tanh($base);
    }

    method acotanh(Complex $x: $base = Radians) {
        (1 / $x).atanh($base);
    }

    multi method polar() {
        $.abs, atan2($.im, $.re);
    }

    method roots(Complex $x: Int $n) {
       return NaN if $n < 1;
       return self if $n == 1;
       return NaN  if $x.re | $x.im ~~  Inf | NaN | -Inf;

       my ($mag, $angle) = $x.polar;
       $mag **= 1 / $n;
       (^$n).map: { $mag.unpolar( ($angle + $_ * 2 * pi) / $n) };
    }

    multi method sign() {
        fail('Cannot take the sign() of a Complex number');
    }

    method sqrt() {
        Q:PIR {
            .local pmc self
            self = find_lex 'self'
            $P0 = get_root_namespace ['parrot'; 'Complex' ]
            $P0 = get_class $P0
            $P0 = $P0.'new'()
            $N0 = self.'re'()
            $P0[0] = $N0
            $N1 = self.'im'()
            $P0[1] = $N1
            $P0 = $P0.'sqrt'()
            $N0 = $P0[0]
            $P2 = box $N0
            $N1 = $P0[1]
            $P3 = box $N1
            $P1 = get_hll_global 'Complex'
            $P1 = $P1.'new'($P2, $P3)
            %r  = $P1
        }
    }

    multi method Num {
        if $!im == 0 {
            $!re;
        } else {
            fail "You can only coerce a Complex to Num if the imaginary part is zero"
        }
    }
}

multi sub infix:<+>(Complex $a, Complex $b) {
    Complex.new($a.re + $b.re, $a.im + $b.im);
}

multi sub infix:<+>(Complex $a, Real $b) {
   Complex.new($a.re + $b, $a.im);
}

multi sub infix:<+>(Real $a, Complex $b) {
    # Was $b + $a; but that trips a ng bug, and also means
    # that Num + Complex is slower than Complex + Num, which
    # seems daft.
    Complex.new($a + $b.re, $b.im);
}

# Originally infix:<-> was implemented in terms of addition, but
# that adds an extra function call to each.  This repeats ourselves,
# but should avoid odd performance anomalies.

multi sub infix:<->(Complex $a, Complex $b) {
    Complex.new($a.re - $b.re, $a.im - $b.im);
}

multi sub infix:<->(Complex $a, Real $b) {
   Complex.new($a.re - $b, $a.im);
}

multi sub infix:<->(Real $a, Complex $b) {
    Complex.new($a - $b.re, -$b.im);
}

multi sub infix:<*>(Complex $a, Complex $b) {
    Complex.new($a.re * $b.re - $a.im * $b.im, $a.im * $b.re + $a.re * $b.im);
}

multi sub infix:<*>(Complex $a, Real $b) {
   Complex.new($a.re * $b, $a.im * $b);
}

multi sub infix:<*>(Real $a, Complex $b) {
    Complex.new($a * $b.re, $a * $b.im);
}

multi sub infix:</>(Complex $a, Complex $b) {
    my $d = $b.re * $b.re + $b.im * $b.im;
    Complex.new(($a.re * $b.re + $a.im * $b.im) / $d,
                ($a.im * $b.re - $a.re * $b.im) / $d);
}

multi sub infix:</>(Complex $a, Real $b) {
    Complex.new($a.re / $b, $a.im / $b);
}

multi sub infix:</>(Real $a, Complex $b) {
    Complex.new($a, 0) / $b;
}

multi sub prefix:<->(Complex $a) {
    Complex.new(-$a.re, -$a.im);
}

multi sub infix:<**>(Complex $a, Complex $b) {
   ($a.log * $b).exp;
}

multi sub infix:<**>(Complex $a, Real $b) {
   ($a.log * $b).exp;
}

multi sub infix:<**>(Real $a, Complex $b) {
    ($a.log * $b).exp;
}

multi sub log(Complex $x) {
    $x.log()
}

multi sub sign(Complex $x) { $x.sign }

# vim: ft=perl6
