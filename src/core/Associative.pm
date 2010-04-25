role Associative[::T = Mu] {
    our multi method postcircumfix:<{ }>() {
        self.values()
    }
    our multi method postcircumfix:<{ }>(*@keys) {
        my $result = pir::new__ps('ResizablePMCArray');
        for @keys {
            pir::push($result, self{$_})
        }
        Q:PIR {
            $P0 = find_lex '$result'
            .tailcall '&infix:<,>'($P0 :flat)
        }
    }
    method of() {
        T
    }
}
