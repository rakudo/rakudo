my class Stash {
    multi method at_key(Hash:D: $key is copy, :$global_fallback) is rw {
        my Mu $storage := nqp::defined(nqp::getattr(self, EnumMap, '$!storage')) ??
            nqp::getattr(self, EnumMap, '$!storage') !!
            nqp::bindattr(self, EnumMap, '$!storage', nqp::hash());
        $key = $key.Str;
        if nqp::existskey($storage, nqp::unbox_s($key)) {
            nqp::atkey($storage, nqp::unbox_s($key))
        }
        elsif $global_fallback {
            nqp::existskey(GLOBAL.WHO, $key)
                ?? GLOBAL.WHO.at_pos($key)
                !! fail("Could not find symbol '$key'")
        }
        else {
            pir::setattribute__0PPsP(my $v, Scalar, '$!whence',
                 -> { nqp::bindkey($storage, nqp::unbox_s($key), $v) } )
        }
    }
}
