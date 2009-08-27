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


# vim: ft=perl6
