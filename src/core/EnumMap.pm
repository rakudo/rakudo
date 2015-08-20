my class X::Hash::Store::OddNumber { ... }

my class EnumMap does Iterable does Associative { # declared in BOOTSTRAP
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
    multi method Int(EnumMap:D:)     { self.elems }
    multi method Numeric(EnumMap:D:) { self.elems }
    multi method Str(EnumMap:D:)     { self.list.Str }

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
          ~ self.pairs.sort.map({.perl}).join(', ')
          ~ ')';
    }

    method iterator(EnumMap:) { self.pairs.iterator }
    method list(EnumMap:) { self.pairs.list }

    multi method pairs(EnumMap:D:) {
        $!storage := nqp::hash() unless $!storage.DEFINITE;
        Seq.new(class :: does Iterator {
            has $!hash-iter;

            method new(\hash) {
                my \iter = self.CREATE;
                nqp::bindattr(iter, self, '$!hash-iter',
                    nqp::iterator(nqp::getattr(hash, EnumMap, '$!storage')));
                iter
            }

            method pull-one() {
                if $!hash-iter {
                    my \tmp = nqp::shift($!hash-iter);
                    Pair.new(key => nqp::iterkey_s(tmp), value => nqp::iterval(tmp))
                }
                else {
                    IterationEnd
                }
            }
        }.new(self))
    }
    multi method keys(EnumMap:D:) {
        $!storage := nqp::hash() unless $!storage.DEFINITE;
        Seq.new(class :: does Iterator {
            has $!hash-iter;

            method new(\hash) {
                my \iter = self.CREATE;
                nqp::bindattr(iter, self, '$!hash-iter',
                    nqp::iterator(nqp::getattr(hash, EnumMap, '$!storage')));
                iter
            }

            method pull-one() {
                $!hash-iter
                    ?? nqp::iterkey_s(nqp::shift($!hash-iter))
                    !! IterationEnd
            }
        }.new(self))
    }
    multi method kv(EnumMap:D:) {
        $!storage := nqp::hash() unless $!storage.DEFINITE;
        Seq.new(class :: does Iterator {
            has $!hash-iter;
            has int $!on-value;

            method new(\hash) {
                my \iter = self.CREATE;
                nqp::bindattr(iter, self, '$!hash-iter',
                    nqp::iterator(nqp::getattr(hash, EnumMap, '$!storage')));
                iter
            }

            method pull-one() {
                if $!on-value {
                    $!on-value = 0;
                    nqp::iterval($!hash-iter)
                }
                elsif $!hash-iter {
                    my \tmp = nqp::shift($!hash-iter);
                    $!on-value = 1;
                    nqp::iterkey_s(tmp)
                }
                else {
                    IterationEnd
                }
            }
        }.new(self))
    }
    multi method values(EnumMap:D:) {
        $!storage := nqp::hash() unless $!storage.DEFINITE;
        Seq.new(class :: does Iterator {
            has $!hash-iter;

            method new(\hash) {
                my \iter = self.CREATE;
                nqp::bindattr(iter, self, '$!hash-iter',
                    nqp::iterator(nqp::getattr(hash, EnumMap, '$!storage')));
                iter
            }

            method pull-one() {
                $!hash-iter
                    ?? nqp::iterval(nqp::shift($!hash-iter))
                    !! IterationEnd
            }
        }.new(self))
    }
    multi method antipairs(EnumMap:D:) {
        self.map: { .value => .key }
    }
    multi method invert(EnumMap:D:) {
        self.map: { (.value »=>» .key).list.Slip }
    }

    multi method AT-KEY(EnumMap:D: \key) is rw {
        my str $skey = nqp::unbox_s(key.Str);
        nqp::defined($!storage) && nqp::existskey($!storage, $skey)
          ?? nqp::atkey($!storage, $skey)
          !! Any
    }

    method STORE(\to_store) {
        my \iter = nqp::istype(to_store, Iterable)
            ?? to_store.iterator
            !! to_store.list.iterator;
        $!storage := nqp::hash();
        until (my Mu $x := iter.pull-one) =:= IterationEnd {
            if nqp::istype($x,Enum) {
                self.STORE_AT_KEY($x.key, $x.value)
            }
            elsif nqp::istype($x, EnumMap) and !nqp::iscont($x) {
                for $x.list { self.STORE_AT_KEY(.key, .value) }
            }
            elsif (my Mu $y := iter.pull-one) !=:= IterationEnd {
                self.STORE_AT_KEY($x, $y)
            }
            else {
                nqp::istype($x,Failure)
                  ?? $x.throw
                  !! X::Hash::Store::OddNumber.new.throw;
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
