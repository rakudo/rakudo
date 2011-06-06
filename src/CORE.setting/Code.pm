my class Code {
    method clone() {
        my $cloned := pir::repr_clone__PP(self);
        pir::setattribute__0PPSP($cloned, Code,
            pir::repr_unbox_str__SP('$!do'),
            pir::perl6_associate_sub_code_object__0PP(
                pir::clone__PP($!do), $cloned))
    }
    
    method derive_dispatcher() {
        my $clone := self.clone();
        Q:PIR {
            $P0 = find_lex 'self'
            $P1 = find_lex 'Code'
            $P0 = getattribute $P0, $P1, '$!do'
            $P1 = getprop 'CLONE_CALLBACK', $P0
            if null $P1 goto no_callback
            $P2 = find_lex '$clone'
            $P1($P0, $P2)
          no_callback:
        };
        pir::setattribute__0PPSP($clone, Code,
            pir::repr_unbox_str__SP('$!dispatchees'),
            pir::clone__PP($!dispatchees))
    }
}
