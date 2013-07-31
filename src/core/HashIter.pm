my class HashIter is Iterator {
    has $!reified;             # Parcel we return after reifying
    has Mu $!hashiter;         # the VM level hash iterator
    has Mu $!keystore;         # key store, if it's a typed hash
    has int $!mode;            # pair = 0, kv = 1, k = 2, v = 3, invert = 4
    
    method new($hash, :$keystore, :$pairs, :$kv, :$k, :$v, :$invert) { 
        my $new := nqp::create(self);
        $new.BUILD($hash, $keystore,
            $pairs  ?? 0 !!
            $kv     ?? 1 !!
            $k      ?? 2 !!
            $v      ?? 3 !!
            $invert ?? 4 !!
                       0);
        $new;
    }

    method BUILD($hash, $keystore, Int $mode) { 
        $!hashiter := nqp::iterator(nqp::getattr(nqp::decont($hash), EnumMap, '$!storage'));
        $!mode      = $mode;
        $!keystore := nqp::getattr(nqp::decont($keystore), EnumMap, '$!storage')
            if $keystore.DEFINITE;
        self 
    }
    
    method reify($n = 1000, :$sink) {
        unless nqp::isconcrete($!reified) {
            my int $count =  nqp::istype($n, Whatever) ?? 1000 !! $n.Int;
            my int $i     =  0;
            my int $mode  =  $!mode;
            my Mu $rpa    := nqp::list();
            my $it        := $!hashiter;
            
            my Mu $pairish;
            if $mode == 0 {
                if nqp::defined($!keystore) {
                    while $it && $i < $count {
                        $pairish := nqp::shift($it);
                        nqp::push($rpa, Pair.new(
                            :key(nqp::atkey($!keystore, nqp::iterkey_s($pairish))),
                            :value(nqp::hllize(nqp::iterval($pairish)))));
                        $i = $i + 1;
                    }
                }
                else {
                    while $it && $i < $count {
                        $pairish := nqp::shift($it);
                        nqp::push($rpa, Pair.new(
                            :key(nqp::p6box_s(nqp::iterkey_s($pairish))),
                            :value(nqp::hllize(nqp::iterval($pairish)))));
                        $i = $i + 1;
                    }
                }
            }
            elsif $mode == 1 {
                if nqp::defined($!keystore) {
                    while $it && $i < $count {
                        $pairish := nqp::shift($it);
                        nqp::push($rpa, nqp::atkey($!keystore, nqp::iterkey_s($pairish)).item);
                        nqp::push($rpa, nqp::hllize(nqp::iterval($pairish)).item);
                        $i = $i + 1;
                    }
                }
                else {
                    while $it && $i < $count {
                        $pairish := nqp::shift($it);
                        nqp::push($rpa, nqp::p6box_s(nqp::iterkey_s($pairish)));
                        nqp::push($rpa, nqp::hllize(nqp::iterval($pairish)).item);
                        $i = $i + 1;
                    }
                }
            }
            elsif $mode == 2 {
                if nqp::defined($!keystore) {
                    while $it && $i < $count {
                        $pairish := nqp::shift($it);
                        nqp::push($rpa, nqp::atkey($!keystore, nqp::iterkey_s($pairish)).item);
                        $i = $i + 1;
                    }
                }
                else {
                    while $it && $i < $count {
                        $pairish := nqp::shift($it);
                        nqp::push($rpa, nqp::p6box_s(nqp::iterkey_s($pairish)));
                        $i = $i + 1;
                    }
                }
            }
            elsif $mode == 3 {
                while $it && $i < $count {
                    $pairish := nqp::shift($it);
                    nqp::push($rpa, nqp::hllize(nqp::iterval($pairish)).item);
                    $i = $i + 1;
                }
            }
            elsif $mode == 4 {
                if nqp::defined($!keystore) {
                    while $it && $i < $count {
                        $pairish := nqp::shift($it);
                        nqp::push($rpa, Pair.new(
                            :value(nqp::atkey($!keystore, nqp::iterkey_s($pairish))),
                            :key(nqp::hllize(nqp::iterval($pairish)))));
                        $i = $i + 1;
                    }
                }
                else {
                    while $it && $i < $count {
                        $pairish := nqp::shift($it);
                        nqp::push($rpa, Pair.new(
                            :value(nqp::p6box_s(nqp::iterkey_s($pairish))),
                            :key(nqp::hllize(nqp::iterval($pairish)))));
                        $i = $i + 1;
                    }
                }
            }
            else {
                die "Unknown hash iteration mode";
            }

            if $it {
                my $nextiter := nqp::create(self);
                nqp::bindattr($nextiter, HashIter, '$!hashiter', $it);
                nqp::bindattr($nextiter, HashIter, '$!keystore', $!keystore);
                nqp::bindattr_i($nextiter, HashIter, '$!mode', $mode);
                nqp::push($rpa, $nextiter);
            }

            $!reified := nqp::p6parcel($rpa, nqp::null());
            # release references to objects we no longer need/own
            $!hashiter := Any;
        }
        $!reified;
    }

    method infinite() { False }

    multi method DUMP(HashIter:D: :$indent-step = 4, :%ctx?) {
        return DUMP(self, :$indent-step) unless %ctx;

        my Mu $attrs := nqp::list();
        nqp::push($attrs, '$!reified'  );
        nqp::push($attrs,  $!reified   );
        nqp::push($attrs, '$!hashiter' );
        nqp::push($attrs,  $!hashiter  );
        nqp::push($attrs, '$!keystore' );
        nqp::push($attrs,  $!keystore  );
        nqp::push($attrs, '$!mode'     );
        nqp::push($attrs,  $!mode      );
        self.DUMP-OBJECT-ATTRS($attrs, :$indent-step, :%ctx);
    }
}

my class EnumMap does Associative { # declared in BOOTSTRAP
    # my class EnumMap is Iterable is Cool {
    #   has $!storage;         # Parrot Hash PMC of key->value mappings

    multi method Bool(EnumMap:D:) {
        nqp::p6bool(nqp::defined($!storage) ?? nqp::elems($!storage) !! 0)
    }
    method elems(EnumMap:) {
        self.DEFINITE && nqp::defined($!storage)
          ?? nqp::p6box_i(nqp::elems($!storage))
          !! 0
    }

    multi method ACCEPTS(EnumMap:D: Any $topic) {
        so self.exists($topic.any);
    }

    multi method ACCEPTS(EnumMap:D: Cool:D $topic) {
        so self.exists($topic);
    }

    multi method ACCEPTS(EnumMap:D: Positional $topic) {
        so self.exists($topic.any);
    }

    multi method ACCEPTS(EnumMap:D: Regex $topic) {
        so self.keys.any.match($topic);
    }
    
    proto method exists(|) {*}
    multi method exists(EnumMap:U:) { False }
    multi method exists(EnumMap:D: Str:D \key) {
        nqp::p6bool(
            nqp::defined($!storage)
            && nqp::existskey($!storage, nqp::unbox_s(key))
        )
    }
    multi method exists(EnumMap:D: \key) {
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

    method fmt(EnumMap: $format = "%s\t\%s", $sep = "\n") {
        self.pairs.fmt($format, $sep);
    }
    
    method hash(\SELF:) is rw {
        SELF
    }
}

multi sub infix:<eqv>(EnumMap:D $a, EnumMap:D $b) {
    if +$a != +$b { return Bool::False }
    for $a.kv -> $k, $v {
        unless $b.exists($k) && $b{$k} eqv $v {
            return Bool::False;
        }
    }
    Bool::True;
}

