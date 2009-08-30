class Int is also {
    our Str multi method Str() {
        ~self;
    }
}

multi sub infix:<+>(Int $a, Int $b) {
    Q:PIR {
        $P0 = find_lex '$a'
        $N0 = $P0
        $P1 = find_lex '$b'
        $N1 = $P1
        $N2 = $N0 + $N1
        %r = '!upgrade_to_num_if_needed'($N2)
    }
}

multi sub infix:<->(Int $a, Int $b) {
    Q:PIR {
        $P0 = find_lex '$a'
        $N0 = $P0
        $P1 = find_lex '$b'
        $N1 = $P1
        $N2 = $N0 - $N1
        %r = '!upgrade_to_num_if_needed'($N2)
    }
}

multi sub infix:<*>(Int $a, Int $b) {
    Q:PIR {
        $P0 = find_lex '$a'
        $N0 = $P0
        $P1 = find_lex '$b'
        $N1 = $P1
        $N2 = $N0 * $N1
        %r = '!upgrade_to_num_if_needed'($N2)
    }
}

multi sub infix:</>(Int $a, Int $b) {
    Q:PIR {
        $P0 = find_lex '$a'
        $N0 = $P0
        $P1 = find_lex '$b'
        $N1 = $P1
        $N2 = $N0 / $N1
        $I2 = floor $N2
        $N3 = $N2 - $I2
      if $N3 != 0 goto notint
        %r = '!upgrade_to_num_if_needed'($N2)
        goto done
      notint:
        %r = box $N2
      done:
    }
}

multi sub infix:<%>(Int $a, Int $b) {
    Q:PIR {
        $P0 = find_lex '$a'
        $N0 = $P0
        $P1 = find_lex '$b'
        $N1 = $P1
        $N2 = mod $N0, $N1
        %r = '!upgrade_to_num_if_needed'($N2)
    }
}

multi sub infix:<**>(Int $a, Int $b) {
    Q:PIR {
        $P0 = find_lex '$a'
        $N0 = $P0
        $P1 = find_lex '$b'
        $N1 = $P1
        $N2 = pow $N0, $N1
        %r = '!upgrade_to_num_if_needed'($N2)
    }
}

multi sub prefix:<->(Int $a) {
    Q:PIR {
        $P0 = find_lex '$a'
        $N0 = $P0
        $N0 = neg $N0
        %r = '!upgrade_to_num_if_needed'($N0)
    }
}

