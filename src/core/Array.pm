class Array is List {
    has $!descriptor;
    
    method at_pos($pos) {
        self.exists($pos)
          ?? pir::find_method__PPs(List, 'at_pos')(self, $pos)
          !! pir::setattribute__0PPsP(my $v, Scalar, '$!whence',
                 -> { pir::find_method__PPs(List, 'BIND_POS')(self, $pos, $v) } )
    }

    method BIND_POS(\$pos, $v is copy) {
        pir::find_method__PPs(List, 'BIND_POS')(self, $pos, $v);
    }

    method STORE(|$) {
        pir::setattribute__vPPsP(self, List, '$!items', Mu);
        pir::shift__PP(
            pir::setattribute__3PPsP(self, List, '$!rest',
                pir::perl6_current_args_rpa__P())
        );
        self.gimme(*);
        self
    }
}

