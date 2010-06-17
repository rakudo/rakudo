role Positional[::T = Mu] {
    our multi method postcircumfix:<[ ]>() { self.list }

    our multi method postcircumfix:<[ ]>(&block) { self[&block(self.elems)]; }

    our multi method postcircumfix:<[ ]>(@pos) {
        my $result = pir::new__ps('ResizablePMCArray');
        for @pos {
            pir::push($result, self[$_])
        }
        Q:PIR {
            $P0 = find_lex '$result'
            %r = '&infix:<,>'($P0 :flat)
        }
    }

    our multi method postcircumfix:<[ ]>($pos) { 
        fail "Cannot use negative index $pos on {self.WHO}" if $pos < 0;
        self.at_pos($pos) 
    }

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
