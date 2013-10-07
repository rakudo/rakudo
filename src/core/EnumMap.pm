my class EnumMap does Associative { # declared in BOOTSTRAP
    # my class EnumMap is Iterable is Cool {
    #   has Mu $!storage;

    multi method Bool(EnumMap:D:) {
        nqp::p6bool(nqp::defined($!storage) ?? nqp::elems($!storage) !! 0)
    }
    method elems(EnumMap:) {
        self.DEFINITE && nqp::defined($!storage)
          ?? nqp::p6box_i(nqp::elems($!storage))
          !! 0
    }

    multi method ACCEPTS(EnumMap:D: Any $topic) {
        so self.exists_key($topic.any);
    }

    multi method ACCEPTS(EnumMap:D: Cool:D $topic) {
        so self.exists_key($topic);
    }

    multi method ACCEPTS(EnumMap:D: Positional $topic) {
        so self.exists_key($topic.any);
    }

    multi method ACCEPTS(EnumMap:D: Regex $topic) {
        so self.keys.any.match($topic);
    }
    
    proto method exists(|) {*}
    multi method exists (EnumMap:U:) {  # is DEPRECATED doesn't work in settings
        DEPRECATED("the :exists adverb");
        self.exists_key;
    }
    multi method exists (EnumMap:D: \key) { # is DEPRECATED doesn't work in settings
        DEPRECATED("the :exists adverb");
        self.exists_key(key);
    }

    proto method exists_key(|) {*}
    multi method exists_key(EnumMap:U:) { False }
    multi method exists_key(EnumMap:D: Str:D \key) {
        nqp::p6bool(
            nqp::defined($!storage)
            && nqp::existskey($!storage, nqp::unbox_s(key))
        )
    }
    multi method exists_key(EnumMap:D: \key) {
        nqp::p6bool(
            nqp::defined($!storage)
            && nqp::existskey($!storage, nqp::unbox_s(key.Stringy))
        )
    }

    multi method perl(EnumMap:D:) {
        self.^name ~ '.new('
            ~ self.keys.map({ .perl ~ ', ' ~ self.at_key($_).perl ~ ', '}).join
            ~ ')';
    }

    method iterator(EnumMap:) { self.pairs.iterator }
    method list(EnumMap:) { self.pairs }

    method keys(EnumMap:) {
        return unless self.DEFINITE && nqp::defined($!storage);
        HashIter.new(self, :k).list
    }
    method kv(EnumMap:) {
        return unless self.DEFINITE && nqp::defined($!storage);
        HashIter.new(self, :kv).list
    }
    method values(EnumMap:) {
        return unless self.DEFINITE && nqp::defined($!storage);
        HashIter.new(self, :v).list
    }
    method pairs(EnumMap:) {
        return unless self.DEFINITE && nqp::defined($!storage);
        HashIter.new(self, :pairs).list
    }
    method invert(EnumMap:) {
        return unless self.DEFINITE && nqp::defined($!storage);
        HashIter.new(self, :invert).list
    }

    method at_key($key) is rw {
        my str $skey = nqp::unbox_s($key.Str);
        nqp::defined($!storage) && nqp::existskey($!storage, $skey)
          ?? nqp::atkey($!storage, $skey)
          !! Any
    }

    method STORE_AT_KEY(\key, Mu \value) is rw {
        nqp::defined($!storage) ||
            nqp::bindattr(self, EnumMap, '$!storage', nqp::hash());
        nqp::bindkey($!storage, nqp::unbox_s(key.Str), value)
    }
    
    method Capture(EnumMap:D:) {
        my $cap := nqp::create(Capture);
        nqp::bindattr($cap, Capture, '$!hash', $!storage);
        $cap
    }
    
    method FLATTENABLE_LIST() { nqp::list() }
    method FLATTENABLE_HASH() {
        nqp::defined($!storage) ||
            nqp::bindattr(self, EnumMap, '$!storage', nqp::hash());
        $!storage
    }

    method fmt(EnumMap: Cool $format = "%s\t\%s", $sep = "\n") {
        if nqp::p6box_i(nqp::sprintfdirectives( nqp::unbox_s($format.Stringy) )) == 1 {
            self.keys.fmt($format, $sep);
        }
        else {
            self.pairs.fmt($format, $sep);
        }
    }
    
    method hash(\SELF:) is rw {
        SELF
    }
}

multi sub infix:<eqv>(EnumMap:D $a, EnumMap:D $b) {
    if +$a != +$b { return Bool::False }
    for $a.kv -> $k, $v {
        unless $b.exists_key($k) && $b{$k} eqv $v {
            return Bool::False;
        }
    }
    Bool::True;
}

