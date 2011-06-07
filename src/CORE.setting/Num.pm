my class Num {
    method Num() { self }
    
    method Int() {
        pir::perl6_box_int__PI(pir::repr_unbox_num__NP(self));
    }
    
    multi method Str(Num:D:) {
        pir::perl6_box_str__PS(pir::repr_unbox_num__NP(self));
    }

    proto method log(|$) {*}
    multi method log() {
        pir::perl6_box_num__PN(pir::ln__NN(pir::repr_unbox_num__NP(self)));
    }
    multi method log(Num \$base) {
        self.log() / $base.log();
    }

    proto method sqrt(|$) {*}
    multi method sqrt() {
        pir::perl6_box_num__PN(pir::sqrt__NN(pir::repr_unbox_num__NP(self)));
    }

    method rand() {
        pir::perl6_box_num__PN(pir::rand__NN(pir::repr_unbox_num__NP(self)));
    }

    method ceil() {
        pir::perl6_box_num__PN(pir::ceil__NN(pir::repr_unbox_num__NP(self)));
    }
    method floor() {
        pir::perl6_box_num__PN(pir::floor__NN(pir::repr_unbox_num__NP(self)));
    }

    proto method sin(|$) {*}
    multi method sin() {
        pir::perl6_box_num__PN(pir::sin__NN(pir::repr_unbox_num__NP(self)));
    }
    proto method asin(|$) {*}
    multi method asin() {
        pir::perl6_box_num__PN(pir::asin__NN(pir::repr_unbox_num__NP(self)));
    }
    proto method cos(|$) {*}
    multi method cos() {
        pir::perl6_box_num__PN(pir::cos__NN(pir::repr_unbox_num__NP(self)));
    }
    proto method acos(|$) {*}
    multi method acos() {
        pir::perl6_box_num__PN(pir::acos__NN(pir::repr_unbox_num__NP(self)));
    }
    proto method tan(|$) {*}
    multi method tan() {
        pir::perl6_box_num__PN(pir::tan__NN(pir::repr_unbox_num__NP(self)));
    }
    proto method atan(|$) {*}
    multi method atan() {
        pir::perl6_box_num__PN(pir::atan__NN(pir::repr_unbox_num__NP(self)));
    }
    proto method sec(|$) {*}
    multi method sec() {
        pir::perl6_box_num__PN(pir::sec__NN(pir::repr_unbox_num__NP(self)));
    }
    proto method asec(|$) {*}
    multi method asec() {
        pir::perl6_box_num__PN(pir::asec__NN(pir::repr_unbox_num__NP(self)));
    }
}
