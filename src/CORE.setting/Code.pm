my class Code {
    method clone() {
        my $cloned := pir::repr_clone__PP(self);
        pir::setattribute__0PPsP($cloned, Code, 
            pir::repr_unbox_str__SP('$!do'),
            pir::clone__PP($!do))
    }
    
    method derive_dispatcher() {
        my $cloned := pir::repr_clone__PP(self);
        pir::setattribute__0PPSP($cloned, Code,
            pir::repr_unbox_str__SP('$!dispatchees'),
            pir::clone__PP($!dispatchees))
    }
}
