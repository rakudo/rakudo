my class Stash { # declared in BOOTSTRAP
    # class Stash is Hash {

    multi method at_key(Stash:D: $key is copy, :$global_fallback) is rw {
        my Mu $storage := nqp::defined(nqp::getattr(self, EnumMap, '$!storage')) ??
            nqp::getattr(self, EnumMap, '$!storage') !!
            nqp::bindattr(self, EnumMap, '$!storage', nqp::hash());
        $key = $key.Str;
        if nqp::existskey($storage, nqp::unbox_s($key)) {
            nqp::atkey($storage, nqp::unbox_s($key))
        }
        elsif $global_fallback {
            nqp::existskey(GLOBAL.WHO, $key)
                ?? GLOBAL.WHO.at_key($key)
                !! fail("Could not find symbol '$key'")
        }
        else {
            nqp::p6bindattrinvres(my $v, Scalar, '$!whence',
                 -> { nqp::bindkey($storage, nqp::unbox_s($key), $v) } )
        }
    }
    
    method package_at_key(Stash:D: str $key) {
        my Mu $storage := nqp::defined(nqp::getattr(self, EnumMap, '$!storage')) ??
            nqp::getattr(self, EnumMap, '$!storage') !!
            nqp::bindattr(self, EnumMap, '$!storage', nqp::hash());
        if nqp::existskey($storage, nqp::unbox_s($key)) {
            nqp::atkey($storage, $key)
        }
        else {
            my $pkg := Metamodel::PackageHOW.new_type(:name($key));
            $pkg.HOW.compose($pkg);
            nqp::bindkey($storage, $key, $pkg)
        }
    }
}
