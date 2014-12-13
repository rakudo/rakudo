my class Metamodel::Primitives {
    method create-type(Mu $how, $repr) {
        nqp::newtype($how, $repr.Str)
    }

    method set-package(Mu $type, $package) {
        nqp::setwho(nqp::decont($type), nqp::decont($package));
        $type
    }

    method install-method-cache(Mu $type, %cache, :$authoritative = True) {
        my Mu $cache := nqp::hash();
        for %cache.kv -> $name, $meth {
            nqp::bindkey($cache, $name, nqp::decont($meth));
        }
        nqp::setmethcache($type, $cache);
        nqp::setmethcacheauth($type, $authoritative ?? 1 !! 0);
        $type
    }

    method configure-type-checking(Mu $type, @cache, :$authoritative = True, :$call-accepts = False) {
        my Mu $cache := nqp::list();
        for @cache {
            nqp::push($cache, nqp::decont($_));
        }
        nqp::settypecache($type, $cache);
        nqp::settypecheckmode($type,
            ($authoritative ?? 0 !! 1) + ($call-accepts ?? 2 !! 0));
        $type
    }

    method configure-destroy(Mu $type, $destroy) {
        nqp::settypefinalize($type, $destroy ?? 1 !! 0);
        $type
    }

    method compose-type(Mu $type, $configuration) {
        multi sub to-vm-types(@array) {
            my Mu $list := nqp::list();
            for @array {
                nqp::push($list, to-vm-types($_));
            }
            $list
        }
        multi sub to-vm-types(%hash) {
            my Mu $hash := nqp::hash();
            for %hash.kv -> $k, $v {
                nqp::bindkey($hash, $k, to-vm-types($v));
            }
            $hash
        }
        multi sub to-vm-types($other) {
            nqp::decont($other)
        }
        nqp::composetype(nqp::decont($type), to-vm-types($configuration));
        $type
    }

    method rebless(Mu $obj, Mu $type) {
        nqp::rebless($obj, $type)
    }

    method is-type(Mu \obj, Mu \type) {
        nqp::p6bool(nqp::istype(obj, type))
    }
}
