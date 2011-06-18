my class Hash {
    # Has attributes and parent EnumMap declared in BOOTSTRAP
    
    method at_key($key) {
        self.exists($key)
          ?? pir::find_method__PPs(EnumMap, 'at_key')(self, $key)
          !! pir::setattribute__0PPsP(my $v, Scalar, '$!whence',
                 -> { pir::find_method__PPs(EnumMap, 'STORE_AT_KEY')(self, $key, $v) } )
    }

    method STORE_AT_KEY(Str \$key, $x is copy) {
        pir::find_method__PPs(EnumMap, 'STORE_AT_KEY')(self, $key, $x);
    }

    method STORE(\$to_store) {
        pir::setattribute__vPPsP(self, EnumMap, '$!storage', 
                                pir::new__Ps('Hash'));
        my $items = $to_store.flat;
        while $items {
            my $key = $items.shift;
            self.STORE_AT_KEY($key, $items.shift);
        }
        self
    }

}

