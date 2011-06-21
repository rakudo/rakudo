my class EnumMap {
    # declared in BOOTSTRAP.pm:
    #   has $!storage;         # Parrot Hash PMC of key->value mappings

    method exists(Str \$key) {
        nqp::p6bool(
            pir::defined($!storage)
            && pir::exists__IQs($!storage, nqp::unbox_s($key))
        )
    }

    method iterator() { self.pairs.iterator }
    method list() { self.pairs }

    method keys()   { self.pairs.map( { $_.key } ) }
    method kv()     { self.pairs.map( { $_.kv } ) }
    method values() { self.pairs.map( { $_.value } ) }
    method pairs() {
        gather {
            my Mu $iter := pir::iter__PP($!storage);
            my Mu $pair;
            while $iter {
                $pair := pir::shift__PP($iter);
                take Pair.new(:key($pair.key), :value($pair.value));
            }
        }
    }

    method at_key(Str \$key) {
        self.exists($key)
            ?? pir::set__PQs($!storage, nqp::unbox_s($key))
            !! Any
    }


    method STORE_AT_KEY(Str \$key, \$value) {
        pir::defined($!storage) ||
            pir::setattribute__vPPsP(self, EnumMap, '$!storage',
                                     pir::new__Ps('Hash'));
        pir::set__2QsP($!storage, nqp::unbox_s($key), $value)
    }
}

