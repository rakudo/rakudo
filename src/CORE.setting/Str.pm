my class Str {
    method Bool() { self ne '' && self ne '0' }
    
    # XXX Should be multi method Str(Str:D $self:) { ... } so we don't
    # screw up the type object stringification.
    method Str() { self }
    method Int() {
        pir::perl6_box_int__PI(pir::repr_unbox_str__SP(self));
    }
    method Num() {
        pir::perl6_box_num__PN(pir::repr_unbox_str__SP(self));
    }
}
