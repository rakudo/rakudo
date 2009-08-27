multi sub infix:<+>(Complex $a, $b) is default {
    Q:PIR {
        $P0 = find_lex '$a'
        $P0 = deobjectref $P0
        $P1 = find_lex '$b'
        $P1 = $P1.'Complex'()
        %r = add $P0, $P1
    }
}

multi sub infix:<+>($a, Complex $b is ref) {
    Q:PIR { 
        $P0 = find_lex '$a' 
        $P0 = $P0.'Complex'()
        $P1 = find_lex '$b'
        $P1 = deobjectref $P1
        %r = add $P0, $P1
    }
}


# vim: ft=perl6
