my class EnumMap {
    # Has attributes and parent Iterable declared in BOOTSTRAP

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


    method STORE_AT_KEY(Str \$key, \$value) {
        pir::defined($!storage) ||
            pir::setattribute__vPPsP(self, EnumMap, '$!storage',
                                     pir::new__Ps('Hash'));
        pir::set__2QsP($!storage, pir::repr_unbox_str__SP($key), $value)
    }
}

