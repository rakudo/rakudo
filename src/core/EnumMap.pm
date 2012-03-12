my class EnumMap does Associative {
    # declared in BOOTSTRAP.pm:
    #   has $!storage;         # Parrot Hash PMC of key->value mappings

    multi method Bool(EnumMap:D:) {
        nqp::p6bool(pir::defined($!storage) ?? nqp::elems($!storage) !! 0)
    }
    method elems(EnumMap:D:) {
        pir::defined($!storage) ?? nqp::p6box_i(nqp::elems($!storage)) !! 0
    }

    multi method ACCEPTS(EnumMap:D: Any $topic) {
        so self.exists($topic.any);
    }

    multi method ACCEPTS(EnumMap:D: Cool:D $topic) {
        so self.exists($topic);
    }
    
    proto method exists(|$) {*}
    multi method exists(EnumMap:D: Str:D \$key) {
        nqp::p6bool(
            pir::defined($!storage)
            && nqp::existskey($!storage, nqp::unbox_s($key))
        )
    }
    multi method exists(EnumMap:D: \$key) {
        nqp::p6bool(
            pir::defined($!storage)
            && nqp::existskey($!storage, nqp::unbox_s($key.Stringy))
        )
    }

    multi method perl(EnumMap:D:) {
        'EnumMap.new('
            ~ self.keys.map({ .perl ~ ', ' ~ self.at_key($_).perl ~ ', '}).join
            ~ ')';
    }

    method iterator() { self.pairs.iterator }
    method list() { self.pairs }

    method keys()   { self.pairs.map( { $_.key } ) }
    method kv()     { self.pairs.map( { $_.kv } ) }
    method values() { self.pairs.map( { $_.value } ) }
    method pairs() {
        return unless pir::defined($!storage);
        gather {
            my Mu $iter := nqp::iterator($!storage);
            my Mu $pair;
            while $iter {
                $pair := nqp::shift($iter);
                take Pair.new(:key($pair.key), :value($pair.value));
            }
            Nil
        }
    }
    method invert() {
        gather {
            my Mu $iter := nqp::iterator($!storage);
            my Mu $pair;
            while $iter {
                $pair := nqp::shift($iter);
                take Pair.new(:key($pair.value), :value($pair.key));
            }
            Nil
        }
    }

    method at_key($key is copy) is rw {
        $key = $key.Str;
        self.exists($key)
            ?? nqp::atkey($!storage, nqp::unbox_s($key))
            !! Any
    }

    method STORE_AT_KEY(\$key, Mu \$value) is rw {
        pir::defined($!storage) ||
            nqp::bindattr(self, EnumMap, '$!storage', pir::new__Ps('Hash'));
        nqp::bindkey($!storage, nqp::unbox_s($key.Str), $value)
    }
    
    method Capture() {
        my $cap := nqp::create(Capture);
        nqp::bindattr($cap, Capture, '$!hash', $!storage);
        $cap
    }
    
    method FLATTENABLE_LIST() { nqp::list() }
    method FLATTENABLE_HASH() {
        pir::defined($!storage) ||
            nqp::bindattr(self, EnumMap, '$!storage', nqp::hash());
        $!storage
    }

    method fmt($format = "%s\t\%s", $sep = "\n") {
        self.pairs.fmt($format, $sep);
    }
}

multi sub infix:<eqv>(EnumMap $a, EnumMap $b) {
    if +$a != +$b { return Bool::False }
    for $a.kv -> $k, $v {
        unless $b.exists($k) && $b{$k} eqv $v {
            return Bool::False;
        }
    }
    Bool::True;
}

