my class Num {
    method Num() { self }
    method Int() {
        pir::repr_box_int__PIP(pir::repr_unbox_num__NP(self), Int);
    }
    method Str() {
        pir::repr_box_str__PSP(pir::repr_unbox_num__NP(self), Str);
    }
}
