my class Int {
    method Bool() {
        pir::perl6_booleanize__PI(
            pir::isne__III(pir::repr_unbox_int__IP(self), pir::repr_unbox_int__IP(0)))
    }
    
    method Int() { self }
    
    multi method Str(Int:D:) {
        pir::perl6_box_str__PS(pir::repr_unbox_int__IP(self));
    }
    
    method Num() {
        pir::perl6_box_num__PN(pir::repr_unbox_int__IP(self));
    }

    method Bridge() {
        self.Num;
    }

    method chr() {
        pir::perl6_box_str__PS(pir::chr(pir::repr_unbox_int__IP(self)));
    }

    proto method succ(|$) {*}
    multi method succ(Int:D:) { self + 1 }
    multi method succ(Int:U:) {        1 }
}

