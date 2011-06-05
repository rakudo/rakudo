my class Code {
    method clone() {
        my $cloned := pir::repr_clone__PP(self);
        pir::setattribute__0PPSP($cloned, Code,
            pir::repr_unbox_str__SP('$!do'),
            pir::perl6_associate_sub_code_object__0PP(
                pir::clone__PP($!do), $cloned))
    }
    
    method derive_dispatcher() {
        pir::setattribute__0PPSP(self.clone(), Code,
            pir::repr_unbox_str__SP('$!dispatchees'),
            pir::clone__PP($!dispatchees))
    }
}
