role Positional[::T = Mu] {
    our multi method postcircumfix:<[ ]>() { self.list }

    our multi method postcircumfix:<[ ]>(&block) { self[&block(self.elems)]; }

    our multi method postcircumfix:<[ ]>($x) { self[$x.Int]; }

    our multi method postcircumfix:<[ ]>(@index) {
        Q:PIR {
            .local pmc result, self, flat
            result = root_new ['parrot';'ResizablePMCArray']
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
            %r = '&infix:<,>'(result :flat)
        }
    }

    our multi method postcircumfix:<[ ]>(Int $index) {
        fail "Cannot use negative index $index on {self.WHO}" if $index < 0;
        Q:PIR {
            .local pmc self, index
            self  = find_lex 'self'
            index = find_lex 'index'
            $I0   = index
            %r    = self[index]
            unless null %r goto done
            %r    = new ['Perl6Scalar']
          done:
        }
    }

    method of() {
        T
    }
}
