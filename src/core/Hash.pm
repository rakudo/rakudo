my class Hash {
    # Has attributes and parent EnumMap declared in BOOTSTRAP
    
    method BIND_KEY(Str \$key, $x is copy) {
        pir::find_method__PPs(EnumMap, 'BIND_KEY')(self, $key, $x);
    }

    method at_key($key) {
        self.exists($key)
          ?? pir::find_method__PPs(EnumMap, 'at_key')(self, $key)
          !! pir::setattribute__0PPsP(my $v, Scalar, '$!whence',
                 -> { pir::find_method__PPs(EnumMap, 'BIND_KEY')(self, $key, $v) } )
    }
}

