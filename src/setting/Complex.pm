multi sub infix:<+>(Complex $a, $b) is default {
    Q:PIR {
        $P0 = find_lex '$a'
        $P1 = find_lex '$b'
        $P1 = $P1.'Complex'()
        $P0 = deobjectref $P0
        $P1 = deobjectref $P1
        %r = add $P0, $P1
    }
}

multi sub infix:<+>($a, Complex $b) {
    Q:PIR { 
        $P0 = find_lex '$a' 
        $P0 = $P0.'Complex'()
        $P1 = find_lex '$b'
        $P0 = deobjectref $P0
        $P1 = deobjectref $P1
        %r = add $P0, $P1
    }
}

multi sub infix:<->(Complex $a, $b) is default {
    Q:PIR {
        $P0 = find_lex '$a'
        $P1 = find_lex '$b'
        $P1 = $P1.'Complex'()
        $P0 = deobjectref $P0
        $P1 = deobjectref $P1
        %r = sub $P0, $P1
    }
}

multi sub infix:<->($a, Complex $b) {
    Q:PIR { 
        $P0 = find_lex '$a' 
        $P0 = $P0.'Complex'()
        $P1 = find_lex '$b'
        $P0 = deobjectref $P0
        $P1 = deobjectref $P1
        %r = sub $P0, $P1
    }
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
    Q:PIR {
        $P0 = find_lex '$a'
        %r = neg $P0
    }
}

multi sub infix:<**>(Complex $a, $b) is default {
    ($a.log * $b).exp;
}

multi sub infix:<**>($a, Complex $b) {
    ($a.log * $b).exp;
}

class Complex is also {
    method sin($base = 'radians') {
        $.re.sin($base) * $.im.cosh($base) + 1i * $.re.cos($base) * $.im.sinh($base);
    }
    
    method cos($base = 'radians') {
        $.re.cos($base) * $.im.cosh($base) - 1i * $.re.sin($base) * $.im.sinh($base);
    }
}

# vim: ft=perl6
