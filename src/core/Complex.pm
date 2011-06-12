# XXX should also be Cool
my class Complex is Numeric {
    # TODO: type as Num?
    has $.re;
    has $.im;
    proto method new(|$) { * }
    multi method new(Real \$re, Real \$im) {
        my $new = self.CREATE;
        $new.BUILD($re, $im);
        $new;
    }
    method BUILD(Real \$re, Real \$im) {
        $!re = $re;
        $!im = $im;
    }
    method reals() {
        (self.re, self.im);
    }

    method isNaN() {
        self.re.isNaN || self.im.isNaN;
    }

    method Real() {
        if $!im == 0 {
            $!re;
        } else {
#            fail "You can only coerce a Complex to Real if the imaginary part is zero"
            Real
        }
    }

    proto method Bool(|$) { * }
    multi method Bool(Complex:D:) {
        $!re != 0e0 || $!im != 0e0;
    }

    method Complex() { self }
    method Str() {
        my $op = $.im < 0 ?? ' - ' !! ' + ';
        $!re.Str ~ $op ~ $!im.abs ~ 'i';
    }

    method perl() {
        "Complex.new($.re, $.im)";
    }
    method conjugate() {
        Complex.new($.re, -$.im);
    }

    method abs(Complex $x:) {
        ($x.re * $x.re + $x.im * $x.im).sqrt
    }

    method polar() {
        $.abs, atan2($.im, $.re);
    }
}

multi sub prefix:<->(Complex \$a) {
    Complex.new(-$a.re, -$a.im);
}

multi sub infix:<+>(Complex \$a, Complex \$b) {
    Complex.new($a.re + $b.re, $a.im + $b.im);
}

multi sub infix:<+>(Complex \$a, Real \$b) {
    Complex.new($a.re + $b, $a.im);
}

multi sub infix:<+>(Real \$a, Complex \$b) {
    Complex.new($a + $b.re, $b.im);
}

multi sub infix:<->(Complex \$a, Complex \$b) {
    Complex.new($a.re - $b.re, $a.im - $b.im);
}

multi sub infix:<->(Complex \$a, Real \$b) {
    Complex.new($a.re - $b, $a.im);
}

multi sub infix:<->(Real \$a, Complex \$b) {
    Complex.new($a - $b.re, -$b.im);
}

multi sub infix:<*>(Complex \$a, Complex \$b) {
    Complex.new($a.re * $b.re - $a.im * $b.im, $a.im * $b.re + $a.re * $b.im);
}

multi sub infix:<*>(Complex \$a, Real \$b) {
    Complex.new($a.re * $b, $a.im * $b);
}

multi sub infix:<*>(Real \$a, Complex \$b) {
    Complex.new($a * $b.re, $a * $b.im);
}

multi sub infix:</>(Complex \$a, Complex \$b) {
    my $d = $b.re * $b.re + $b.im * $b.im;
    Complex.new(($a.re * $b.re + $a.im * $b.im) / $d,
                ($a.im * $b.re - $a.re * $b.im) / $d);
}

multi sub infix:</>(Complex \$a, Real \$b) {
    Complex.new($a.re / $b, $a.im / $b);
}

multi sub infix:</>(Real \$a, Complex \$b) {
    Complex.new($a, 0) / $b;
}

proto postfix:<i>(|$) { * }
multi postfix:<i>(Real    \$a) { Complex.new(0e0, $a);     }
multi postfix:<i>(Complex \$a) { Complex.new(-$a.re, $a.im) }
multi postfix:<i>(Numeric \$a) { $a * 1i }

# vim: ft=perl6
