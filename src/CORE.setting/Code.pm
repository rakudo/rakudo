my class Code {
    method clone() {
        my $cloned := pir::repr_clone__PP(self);
        # XXX Kinda want to write what's below...but '$!do' won't
        # Parrot stringify just yet.
        # pir::set_attribute__0PPsP($cloned, Code, '$!do', pir::clone__PP($!do))
        Q:PIR {
            %r = find_lex '$cloned'
            $P0 = find_lex 'Code'
            $P1 = getattribute %r, $P0, '$!do'
            $P1 = clone $P1
            setattribute %r, $P0, '$!do', $P1
        }
    }
}
