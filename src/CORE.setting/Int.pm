my class Int {
    method Int() { self }
    
    # XXX multi method Str(Int:D $self:) { ...
    method Str() {
        pir::repr_box_str__PSP(pir::repr_unbox_int__IP(self), Str);
    }
}
