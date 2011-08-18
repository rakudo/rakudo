my class Capture {
    has Mu $!list;
    has Mu $!hash;

    submethod BUILD(:$!list, :$!hash) { }

    method at_key(Capture:D: $key is copy) {
        $key = $key.Str;
        nqp::existskey($!hash, nqp::unbox_s($key))
          ?? nqp::atkey($!hash, nqp::unbox_s($key))
          !! Any
    }

    method at_pos(Capture:D: $pos is copy) {
        $pos = $pos.Int;
        nqp::existspos($!list, nqp::unbox_i($pos))
          ?? nqp::atpos($!list, nqp::unbox_i($pos))
          !! Any
    }

    method hash(Capture:D:) {
        my $enum := nqp::create(EnumMap);
        nqp::bindattr($enum, EnumMap, '$!storage', $!hash);
        $enum;
    }

    method list(Capture:D:) {
        nqp::p6list(pir::clone__PP($!list), List, Mu);
    }
    
    method elems(Capture:D:) {
        nqp::p6box_i(nqp::elems($!list))
    }
    
    multi method gist(Capture:D:) {
        my Mu $gist := nqp::list();
        if $!list {
            my Mu $iter := nqp::iterator($!list);
            nqp::push($gist, nqp::unbox_s(nqp::shift($iter).gist)) while $iter;
        }
        if $!hash {
            my Mu $iter := nqp::iterator($!hash);
            while $iter {
                my $kv := nqp::shift($iter);
                nqp::push($gist, nqp::unbox_s((nqp::p6box_s($kv) => $kv.value).gist));
            }
        }
        nqp::p6box_s(nqp::join(' ', $gist))
    }
    
    multi method Str(Capture:D:) {
        my Mu $str := nqp::list();
        if $!list {
            my Mu $iter := nqp::iterator($!list);
            nqp::push($str, nqp::unbox_s(nqp::shift($iter).Str)) while $iter;
        }
        if $!hash {
            my Mu $iter := nqp::iterator($!hash);
            while $iter {
                my $kv := nqp::shift($iter);
                nqp::push($str, nqp::unbox_s((nqp::p6box_s($kv) => $kv.value).Str));
            }
        }
        nqp::p6box_s(nqp::join(' ', $str))
    }
    
    method Capture(Capture:D:) {
        self
    }
    
    multi method Numeric(Capture:D:) {
        self.elems
    }
    
    # XXX TODO: Should include the hash part too...
    method ARGLIST_FLATTENABLE() { $!list }

    method pairs(Capture:D:) {
        (self.list.pairs, self.hash.pairs).flat
    }
    method values(Capture:D:) {
        (self.list.values, self.hash.values).flat
    }
    method keys(Capture:D:) {
        (self.list.keys, self.hash.keys).flat
    }
    method kv(Capture:D:) {
        (self.list.kv, self.hash.kv).flat
    }
}

multi sub infix:<eqv>(Capture $a, Capture $b) {
    $a.WHAT === $b.WHAT && $a.list eqv $b.list && $a.hash eqv $b.hash
}

