my class Num {
    method Num() { self }
    method Int() {
        pir::perl6_box_int__PI(pir::repr_unbox_num__NP(self));
    }
    method Str() {
        pir::perl6_box_str__PS(pir::repr_unbox_num__NP(self));
    }
}
