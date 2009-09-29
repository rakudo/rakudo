class Complex {
    has $.re;
    has $.im;

    multi method new($re, $im) {
        self.bless(*, :$re, :$im);
    }

    multi method perl() { "Complex.new($.re, $.im)"; }

    multi method Str() { "$.re + {$.im}i"; }

    # multi method sin($base = 'radians') {
    #     $.re.sin($base) * $.im.cosh($base) + ($.re.cos($base) * $.im.sinh($base))i;
    # }
    #
    # multi method cos($base = 'radians') {
    #     $.re.cos($base) * $.im.cosh($base) - ($.re.sin($base) * $.im.sinh($base))i;
    # }

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

multi sub infix:<*>(Complex $a, $b) is default {
    Q:PIR {
        $P0 = find_lex '$a'
        $P1 = find_lex '$b'
        $P1 = $P1.'Complex'()
        $P0 = deobjectref $P0
        $P1 = deobjectref $P1
        %r = mul $P0, $P1
    }
}

multi sub infix:<*>($a, Complex $b) {
    Q:PIR { 
        $P0 = find_lex '$a' 
        $P0 = $P0.'Complex'()
        $P1 = find_lex '$b'
        $P0 = deobjectref $P0
        $P1 = deobjectref $P1
        %r = mul $P0, $P1
    }
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


multi sub prefix:<->(Complex $a) {
    Complex.new(-$a.re, -$a.im);
}

multi sub infix:<**>(Complex $a, $b) is default {
    ($a.log * $b).exp;
}

multi sub infix:<**>($a, Complex $b) {
    ($a.log * $b).exp;
}

# vim: ft=perl6
