class Array is List {
    has $!descriptor;
    
    method BIND_POS(\$pos, $v is copy) {
        pir::find_method__PPs(List, 'BIND_POS')(self, $pos, $v);
    }

    method at_pos($pos) {
        self.exists($pos)
          ?? pir::find_method__PPs(List, 'at_pos')(self, $pos)
          !! pir::setattribute__0PPsP(my $v, Scalar, '$!whence',
                 -> { pir::find_method__PPs(List, 'BIND_POS')(self, $pos, $v) } )
    }
}

