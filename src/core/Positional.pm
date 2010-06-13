role Positional[::T = Mu] {
    our multi method postcircumfix:<[ ]>() { self.list }

    our multi method postcircumfix:<[ ]>(Int $index) {
        Q:PIR {
            .local pmc result
            $P0 = find_lex '$index'
            $I0 = $P0
            $P0 = find_lex 'self'
            result = $P0[$I0]
            unless null result goto have_result
            result = new ['Perl6Scalar']
          have_result:
            %r = result
        }
    }

    method of() {
        T
    }
}
