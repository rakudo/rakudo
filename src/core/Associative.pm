role Associative[::T = Mu] {
    our multi method postcircumfix:<{ }>() {
        self.values()
    }

    our multi method postcircumfix:<{ }>(@keys) {
        my $result = pir::new__ps('ResizablePMCArray');
        for @keys {
            pir::push($result, self{$_})
        }
        Q:PIR {
            $P0 = find_lex '$result'
            %r = '&infix:<,>'($P0 :flat)
        }
    }

    our multi method postcircumfix:<{ }>($key) { self.at_key($key) }

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
