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
}
