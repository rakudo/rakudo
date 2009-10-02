class Complex {
    has $.re;
    has $.im;

    multi method new($re, $im) {
        self.bless(*, :$re, :$im);
    }

    multi method abs() {
        ($!re * $!re + $!im * $!im).sqrt
    }

    multi method Complex() { self }

    multi method perl() {
        "Complex.new($.re, $.im)";
    }

    multi method Str() {
        "$.re + {$.im}i";
    }

    multi method exp() {
        Complex.new($.re.Num.exp * $.im.Num.cos, $.re.Num.exp * $.im.Num.sin);
    }

    multi method sin($base = 'radians') {
        $.re.sin($base) * $.im.cosh($base) + ($.re.cos($base) * $.im.sinh($base))i;
    }

    multi method asin($base = 'radians') {
        (-1i * log((self)i + sqrt(1 - self * self)))!from-radians($base);
    }

    multi method cos($base = 'radians') {
        $.re.cos($base) * $.im.cosh($base) - ($.re.sin($base) * $.im.sinh($base))i;
    }

    multi method acos($base = 'radians') {
       (pi / 2)!from-radians($base) - self.asin($base);
    }

    multi method tan($base = 'radians') {
        self.sin($base) / self.cos($base);
    }

    multi method atan($base = 'radians') {
       ((log(1 - (self)i) - log(1 + (self)i))i / 2)!from-radians($base);
    }

    multi method log() {
        Q:PIR {
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

    multi method log10() {
        $.log / 10.log;
    }

    multi method polar() {
        $.abs, atan2($.im, $.re);
    }

    multi method roots($n is copy) {
        my ($mag, $angle) = @.polar;
	return NaN  if $n < 1;
	return self if $n == 1;
	return NaN  if $!re|$!im ~~  Inf|NaN|-Inf; 
	$n = $n.Int;
        $mag **= 1/$n;
        (^$n).map: { $mag.unpolar( ($angle + $_ * 2 * pi) / $n) };
    }

    multi method sqrt() {
        Q:PIR {
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

    multi method cosec($base = 'radians') {
        1.0 / self!to-radians($base).sin;
    }

    multi method cosech($base = 'radians') {
        1.0 / self!to-radians($base).sinh;
    }

    multi method acosec($base = 'radians') {
        (1.0 / self).asin!to-radians($base);
    }

    multi method cotan($base = 'radians') {
        1.0 / self!to-radians($base).tan;
    }

    multi method cotanh($base = 'radians') {
        1.0 / self!to-radians($base).tanh;
    }

    multi method acotan($base = 'radians') {
        (1.0 / self).atan!to-radians($base);
    }

    multi method acosech($base = 'radians') {
        (1.0 / self).asinh!to-radians($base);
    }

    multi method acotanh($base = 'radians') {
        (1.0 / self).atanh!to-radians($base);
    }

    multi method Num {
	if $!im == 0 {
	     $!re;
	} else {
	     fail "You can only coerce a Complex to Num if the imaginary part is zero"
	}
    }
}

multi sub abs(Complex $x) { $x.abs }

multi sub infix:<+>(Complex $a, Complex $b) {
    Complex.new($a.re + $b.re, $a.im + $b.im);
}

multi sub infix:<+>(Complex $a, $b) is default {
    Complex.new($a.re + $b, $a.im);
}

multi sub infix:<+>($a, Complex $b) {
    $b + $a;
}

multi sub infix:<->(Complex $a, $b) is default {
    $a + (-$b);
}

multi sub infix:<->($a, Complex $b) {
    $a + (-$b);
}

multi sub infix:<*>(Complex $a, Complex $b) {
    Complex.new($a.re * $b.re - $a.im * $b.im, $a.im * $b.re + $a.re * $b.im);
#    Complex.new($a.re * $a.re - $a.im * $b.im, $a.re * $b.im + $a.im * $b.re);
}

multi sub infix:<*>(Complex $a, $b) is default {
    Complex.new($a.re * $b, $a.im * $b);

}

multi sub infix:<*>($a, Complex $b) {
    Complex.new($a * $b.re, $a * $b.im);
}

multi sub infix:</>(Complex $a, Complex $b) {
    my $d = $b.re * $b.re + $b.im * $b.im;
    Complex.new(($a.re * $b.re + $a.im * $b.im) / $d,
                ($a.im * $b.re - $a.re * $b.im) / $d);
}

multi sub infix:</>(Complex $a, $b) is default {
    $a * (1/$b);
}

multi sub infix:</>($a, Complex $b) {
    Complex.new($a, 0) / $b;
}

multi sub postfix:<i>($x) {
    Complex.new(0, +$x);
}

multi sub postfix:<i>(Complex $z) {
    Complex.new(-$z.im, $z.re);
}

multi sub prefix:<->(Complex $a) {
    Complex.new(-$a.re, -$a.im);
}

multi sub infix:<**>(Complex $a, $b) is default {
    ($a.log * $b).exp;
}

multi sub infix:<**>($a, Complex $b) {
    ($a.log * $b).exp;
}

multi sub log(Complex $x) {
    $x.log()
}

multi sub log10(Complex $x) {
    $x.log10;
}

multi sub sqrt(Complex $x) {
    $x.sqrt;
}

multi sub exp(Complex $x) {
    $x.exp()
}

# vim: ft=perl6
