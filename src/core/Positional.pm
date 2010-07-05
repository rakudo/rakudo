role Positional[::T = Mu] {

    method at_pos($pos) {
        Q:PIR {
            .local pmc self, pos
            self = find_lex 'self'
            pos  = find_lex '$pos'
            $I0  = pos
            %r   = self[$I0]
            unless null %r goto done
            %r   = new ['Perl6Scalar']
          done:
        }
    }

    method of() {
        T
    }
}
