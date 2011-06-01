role Associative[::T = Mu] {
    method at_key($key) {
        Q:PIR {
            .local pmc self, key
            self = find_lex 'self'
            key  = find_lex '$key'
            %r   = self[key]
            unless null %r goto done
            %r   = new ['Perl6Scalar']
          done:
        }
    }

    method of() {
        T
    }
}
