augment class Array {

    our method push(*@values) is export {
        my @a = @values;
        Q:PIR {
            $P0 = find_lex '@a'
            $P0 = getattribute $P0, '$!values'
            $P1 = find_lex 'self'
            $P2 = $P1.'!generate'(0)
            $I0 = elements $P2
            splice $P2, $P0, $I0, 0
        };
        self;
    }

    our method unshift(*@values) is export {
        my @a = @values;
        Q:PIR {
            $P0 = find_lex '@a'
            $P0 = getattribute $P0, '$!values'
            $P1 = find_lex 'self'
            $P2 = $P1.'!generate'(0)
            splice $P2, $P0, 0, 0
            $P0 = box 0
            setattribute $P1, '$!gen', $P0
        };
        self;
    }

    #CHEAT: Should return fail rather than Mu
    our multi method pop() {
        self.elems > 0 ?? pir::pop__PP($!values) !! Mu;
    }

    #CHEAT: Should return fail rather than Mu
    our multi method shift() {
        self.elems > 0 ?? pir::shift__PP($!values) !! Mu;
    }
}

our proto sub pop(@array) { @array.pop; }
our proto sub shift(@array) { @array.shift; }
