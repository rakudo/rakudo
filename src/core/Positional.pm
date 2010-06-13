role Positional[::T = Mu] {
    our multi method postcircumfix:<[ ]>() { self.list }

    our multi method postcircumfix:<[ ]>(@index) {
        Q:PIR {
            .local pmc result, self, flat
            result = new ['Parcel']
            self = find_lex 'self'
            $P0 = find_lex '@index'
            $P0 = $P0.'flat'()
            flat = $P0.'eager'()
          loop:
            unless flat goto done
            $P0 = shift flat
            $P0 = '!postcircumfix:<[ ]>'(self, $P0)
            push result, $P0
            goto loop
          done:
            %r = result
        }
    }

    method of() {
        T
    }
}
