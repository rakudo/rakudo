augment class Array {
   method perl() {
        # XXX: $_.perl and .perl don't work, but this does...
        '[' ~ self.map({ $^a.perl }).join(', ') ~ ']';
    }
 

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

    our multi method pop() {
        self.elems > 0 ?? pir::pop__PP($!values)
                       !! fail('Undefined value popped from empty array');
    }

    our multi method shift() {
        self.elems > 0 ?? pir::shift__PP($!values)
                       !! fail('Undefined value shifted from empty array');
    }
}

our proto sub pop(@array) { @array.pop; }
our proto sub shift(@array) { @array.shift; }
