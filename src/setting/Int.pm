class Int is also {
    our Int multi method Int() { self }

    our Num multi method Num() {
        Q:PIR {
            $N0 = self
            %r = box $N0
        }
    }
 
    our Rat multi method Rat() { Rat.new(self, 1); }

    our Str multi method Str() {
        ~self;
    }

    # Most of the trig functions for Int are in Any-num.pm, but
    # sec is a special case.
    our Num multi method sec($base = 'radians') is export {
        self.Num.sec($base);
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

multi sub infix:<div>(Int $a, Int $b) {
    Q:PIR {
        $P0 = find_lex '$a'
        $I0 = $P0
        $P1 = find_lex '$b'
        $I1 = $P1
        $I2 = $I0 / $I1
        %r = box $I2
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

