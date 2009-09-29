class Complex {
    has $.re;
    has $.im;

    multi method new($re, $im) {
        self.bless(*, :$re, :$im);
    }

    multi method perl() {
        "Complex.new($.re, $.im)";
    }

    multi method Str() {
        "$.re + {$.im}i";
    }

    multi method exp() is export {
        Complex.new($.re.Num.exp * $.im.Num.cos, $.re.Num.exp * $.im.Num.sin);
    }

    # multi method sin($base = 'radians') {
    #     $.re.sin($base) * $.im.cosh($base) + ($.re.cos($base) * $.im.sinh($base))i;
    # }
    #
    # multi method cos($base = 'radians') {
    #     $.re.cos($base) * $.im.cosh($base) - ($.re.sin($base) * $.im.sinh($base))i;
    # }

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

}

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

multi sub infix:</>(Complex $a, $b) is default {
    Q:PIR {
        $P0 = find_lex '$a'
        $P1 = find_lex '$b'
        $P1 = $P1.'Complex'()
        $P0 = deobjectref $P0
        $P1 = deobjectref $P1
        %r = div $P0, $P1
    }
}

multi sub infix:</>($a, Complex $b) {
    Q:PIR { 
        $P0 = find_lex '$a' 
        $P0 = $P0.'Complex'()
        $P1 = find_lex '$b'
        $P0 = deobjectref $P0
        $P1 = deobjectref $P1
        %r = div $P0, $P1
    }
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

# vim: ft=perl6
