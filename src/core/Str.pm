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

    multi method ACCEPTS(Str:D: $other) { $other eq self }

    # XXX: need to translate escapes
    multi method perl(Str:D:) { "'" ~ self ~ "'" }

}

proto infix:<x>(|$) {*}
multi infix:<x>(Str $s, Int $repetition) {
    pir::perl6_box_str__PS(
        pir::repeat__SSI(
            pir::repr_unbox_str__SP($s),
            pir::repr_unbox_int__IP($repetition),
        )
    );
}
multi infix:<x>($s, $repetition) {
    $s.Str x $repetition.Int
}
