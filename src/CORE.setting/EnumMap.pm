my class EnumMap is Iterable {
    has $!storage;

    method exists(Str \$key) {
        pir::perl6_booleanize__PI(
            pir::defined($!storage)
            && pir::exists__IQs($!storage, pir::repr_unbox_str__SP($key))
        )
    }

    method at_key(Str \$key) {
        self.exists($key)
            ?? pir::set__PQs($!storage, pir::repr_unbox_str__SP($key))
            !! Any
    }


    method BIND_KEY(Str \$key, \$value) {
        pir::defined($!storage) ||
            pir::setattribute__vPPsP(self, EnumMap, '$!storage',
                                     pir::new__Ps('Hash'));
        pir::set__2QsP($!storage, pir::repr_unbox_str__SP($key), $value)
    }
}

