my class Int {
    method Bool() {
        pir::perl6_booleanize__PI(
            pir::isne__III(pir::repr_unbox_int__IP(self), pir::repr_unbox_int__IP(0)))
    }
    
    method Int() { self }
    
    # XXX multi method Str(Int:D $self:) { ...
    method Str() {
        pir::repr_box_str__PSP(pir::repr_unbox_int__IP(self), Str);
    }
    method Num() {
        pir::repr_box_num__PNP(pir::repr_unbox_int__IP(self), Num);
    }
}

