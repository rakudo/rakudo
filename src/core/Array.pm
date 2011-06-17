class Array {
    # Has attributes and parent List declared in BOOTSTRAP.    
    
    method at_pos($pos) {
        self.exists($pos)
          ?? pir::find_method__PPs(List, 'at_pos')(self, $pos)
          !! pir::setattribute__0PPsP(my $v, Scalar, '$!whence',
                 -> { pir::find_method__PPs(List, 'STORE_AT_POS')(self, $pos, $v) } )
    }

    method STORE_AT_POS(\$pos, $v is copy) {
        pir::find_method__PPs(List, 'STORE_AT_POS')(self, $pos, $v);
    }

    method STORE(|$) {
        # get arguments, shift off invocant
        my $args := pir::perl6_current_args_rpa__P();
        pir::shift__PP($args);
        # clear our current items, and create a flattening iterator
        # that will bring in values from $args
        pir::setattribute__vPPsP(self, List, '$!items', Mu);
        pir::setattribute__0PPsP(self, List, '$!nextiter',
            pir::perl6_iter_from_rpa__PPPP($args, self, 1.Bool));
        self.eager
    }
}

