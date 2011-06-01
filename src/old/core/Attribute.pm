augment class Attribute {
    method has-accessor() {
        $!has_accessor ?? True !! False
    }
    method Str() { self.name }

    method get_value(Mu $obj) {
        pir::getattribute__PPS($obj, self.name);
    }
    method set_value(Mu $obj, Mu $new_val) {
        pir::setattribute__vPSP($obj,  self.name, $new_val);
    }
}
