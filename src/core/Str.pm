my class Str {
    method Bool() { self ne '' && self ne '0' }
    
    multi method Str(Str:D:) { self }
    
    method Int() {
        pir::perl6_box_int__PI(pir::repr_unbox_str__SP(self));
    }
    
    method Num() {
        pir::perl6_box_num__PN(pir::repr_unbox_str__SP(self));
    }

    method Numeric() { self.Num }
}
