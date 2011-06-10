my class Hash is EnumMap {
    method BIND_Key(Str \$key, $x is copy) {
        pir::find_method__PPs(EnumMap, 'BIND_KEY')(self, $key, $x);
    }

    method at_key($key) {
        self.exists($key)
          ?? pir::find_method__PPs(EnumMap, 'at_key')(self, $key)
          !! pir::setattribute__0PPsP(my $v, Scalar, '$!whence',
                 -> { pir::find_method__PPs(EnumMap, 'BIND_KEY')(self, $key, $v) } )
    }
}

