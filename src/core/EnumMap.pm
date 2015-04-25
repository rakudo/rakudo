my class X::Hash::Store::OddNumber { ... }

my class EnumMap does Associative { # declared in BOOTSTRAP
    # my class EnumMap is Iterable is Cool {
    #   has Mu $!storage;

    method new(*@args) {
        my %h := nqp::create(self);
        %h.STORE(@args) if @args;
        %h;
    }

    multi method Bool(EnumMap:D:) {
        nqp::p6bool(nqp::defined($!storage) && nqp::elems($!storage));
    }
    method elems(EnumMap:D:) {
        nqp::p6box_i(nqp::defined($!storage) && nqp::elems($!storage));
    }

    multi method ACCEPTS(EnumMap:D: Any $topic) {
        self.EXISTS-KEY($topic.any);
    }

    multi method ACCEPTS(EnumMap:D: Cool:D $topic) {
        self.EXISTS-KEY($topic);
    }

    multi method ACCEPTS(EnumMap:D: Positional $topic) {
        self.EXISTS-KEY($topic.any);
    }

    multi method ACCEPTS(EnumMap:D: Regex $topic) {
        so self.keys.any.match($topic);
    }

    multi method EXISTS-KEY(EnumMap:D: Str:D \key) {
        nqp::p6bool(
            nqp::defined($!storage)
            && nqp::existskey($!storage, nqp::unbox_s(key))
        )
    }
    multi method EXISTS-KEY(EnumMap:D: \key) {
        nqp::p6bool(
            nqp::defined($!storage)
            && nqp::existskey($!storage, nqp::unbox_s(key.Str))
        )
    }

    multi method perl(EnumMap:D:) {
        self.^name
          ~ '.new('
          ~ self.pairs.pick(*,:eager).map({.perl}).join(', ')
          ~ ')';
    }

    method iterator(EnumMap:) { self.pairs.iterator }
    method list(EnumMap:) { self.pairs }

    multi method keys(EnumMap:D:) {
        (nqp::defined($!storage) ?? HashIter.keys(self)     !! ()).list;
    }
    multi method kv(EnumMap:D:) {
        (nqp::defined($!storage) ?? HashIter.kv(self)       !! ()).list;
    }
    multi method values(EnumMap:D:) {
        (nqp::defined($!storage) ?? HashIter.values(self)   !! ()).list;
    }
    multi method pairs(EnumMap:D:) {
        (nqp::defined($!storage) ?? HashIter.pairs(self)    !! ()).list;
    }
    multi method antipairs(EnumMap:D:) {
        (nqp::defined($!storage) ?? HashIter.antipairs(self) !! ()).list;
    }
    multi method invert(EnumMap:D:) {
        (nqp::defined($!storage) ?? HashIter.invert(self)   !! ()).list;
    }

    multi method AT-KEY(EnumMap:D: \key) is rw {
        my str $skey = nqp::unbox_s(key.Str);
        nqp::defined($!storage) && nqp::existskey($!storage, $skey)
          ?? nqp::atkey($!storage, $skey)
          !! Any
    }

    method STORE(\to_store) {
        my $items = (to_store,).flat.eager;
        $!storage := nqp::hash();

        if $items.elems == 1 {
            if nqp::istype($items[0],EnumMap) {
                my Mu $x := $items.shift;
                DEPRECATED(
                  self.VAR.name ~ ' = %(itemized hash)',
                  |<2014.07 2015.07>,
                  :what(self.VAR.name ~ ' = itemized hash')
                ) if nqp::iscont($x);
                for $x.list { self.STORE_AT_KEY(.key, .value) }
                return self;
            }
        }

        while $items {
            my Mu $x := $items.shift;
            if nqp::istype($x,Enum) { self.STORE_AT_KEY($x.key, $x.value) }
            elsif nqp::istype($x,EnumMap) and !nqp::iscont($x) {
                for $x.list { self.STORE_AT_KEY(.key, .value) }
            }
            elsif $items { self.STORE_AT_KEY($x, $items.shift) }
            else {
                X::Hash::Store::OddNumber.new.throw
            }
        }
        self
    }

    proto method STORE_AT_KEY(|) is rw { * }
    multi method STORE_AT_KEY(Str \key, Mu \value) is rw {
        nqp::defined($!storage) ||
            nqp::bindattr(self, EnumMap, '$!storage', nqp::hash());
        nqp::bindkey($!storage, nqp::unbox_s(key), value)
    }
    multi method STORE_AT_KEY(\key, Mu \value) is rw {
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

    method hash() { self }
}

multi sub infix:<eqv>(EnumMap:D $a, EnumMap:D $b) {
    if +$a != +$b { return Bool::False }
    for $a.kv -> $k, $v {
        unless $b.EXISTS-KEY($k) && $b{$k} eqv $v {
            return Bool::False;
        }
    }
    Bool::True;
}


# vim: ft=perl6 expandtab sw=4
