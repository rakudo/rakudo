my class X::Hash::Store::OddNumber { ... }

my class Map does Iterable does Associative { # declared in BOOTSTRAP
    # my class Map is Iterable is Cool {
    #   has Mu $!storage;

    method new(*@args) {
        my %h := nqp::create(self);
        %h.STORE(@args) if @args;
        %h;
    }

    multi method Bool(Map:D:) {
        nqp::p6bool(nqp::defined($!storage) && nqp::elems($!storage));
    }
    method elems(Map:D:) {
        nqp::p6box_i(nqp::defined($!storage) && nqp::elems($!storage));
    }
    multi method Int(Map:D:)     { self.elems }
    multi method Numeric(Map:D:) { self.elems }
    multi method Str(Map:D:)     { self.list.Str }

    multi method ACCEPTS(Map:D: Any $topic) {
        self.EXISTS-KEY($topic.any);
    }

    multi method ACCEPTS(Map:D: Cool:D $topic) {
        self.EXISTS-KEY($topic);
    }

    multi method ACCEPTS(Map:D: Positional $topic) {
        self.EXISTS-KEY($topic.any);
    }

    multi method ACCEPTS(Map:D: Regex $topic) {
        so self.keys.any.match($topic);
    }

    multi method EXISTS-KEY(Map:D: Str:D \key) {
        nqp::p6bool(
            nqp::defined($!storage)
            && nqp::existskey($!storage, nqp::unbox_s(key))
        )
    }
    multi method EXISTS-KEY(Map:D: \key) {
        nqp::p6bool(
            nqp::defined($!storage)
            && nqp::existskey($!storage, nqp::unbox_s(key.Str))
        )
    }

    multi method perl(Map:D:) {
        self.^name
          ~ '.new('
          ~ self.pairs.sort.map({.perl}).join(', ')
          ~ ')';
    }

    method iterator(Map:) { self.pairs.iterator }
    method list(Map:) { self.pairs.cache }

    multi method pairs(Map:D:) {
        Seq.new(class :: does Rakudo::Internals::MapIterator {
            method pull-one() {
                if $!hash-iter {
                    my \tmp = nqp::shift($!hash-iter);
                    Pair.new(nqp::iterkey_s(tmp), nqp::iterval(tmp))
                }
                else {
                    IterationEnd
                }
            }
            method push-all($target) {
                while $!hash-iter {
                    my \tmp = nqp::shift($!hash-iter);
                    $target.push(
                      Pair.new(nqp::iterkey_s(tmp), nqp::iterval(tmp)));
                }
                IterationEnd
            }
        }.new(self))
    }
    multi method keys(Map:D:) {
        Seq.new(class :: does Rakudo::Internals::MapIterator {
            method pull-one() {
                $!hash-iter
                    ?? nqp::iterkey_s(nqp::shift($!hash-iter))
                    !! IterationEnd
            }
            method push-all($target) {
                $target.push(nqp::iterkey_s(nqp::shift($!hash-iter)))
                  while $!hash-iter;
                IterationEnd
            }
        }.new(self))
    }
    multi method kv(Map:D:) {
        Seq.new(class :: does Rakudo::Internals::MapIterator {
            has int $!on-value;

            method pull-one() is raw {
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
            method push-all($target) {
                while $!hash-iter {
                    my \tmp = nqp::shift($!hash-iter);
                    $target.push(nqp::iterkey_s(tmp));
                    $target.push(nqp::iterval(tmp));
                }
                IterationEnd
            }
        }.new(self))
    }
    multi method values(Map:D:) {
        Seq.new(class :: does Rakudo::Internals::MapIterator {
            method pull-one() is raw {
                $!hash-iter
                    ?? nqp::iterval(nqp::shift($!hash-iter))
                    !! IterationEnd
            }
            method push-all($target) {
                $target.push(nqp::iterval(nqp::shift($!hash-iter)))
                  while $!hash-iter;
                IterationEnd
            }
        }.new(self))
    }
    multi method antipairs(Map:D:) {
        Seq.new(class :: does Rakudo::Internals::MapIterator {
            method pull-one() {
                if $!hash-iter {
                    my \tmp = nqp::shift($!hash-iter);
                    Pair.new( nqp::iterval(tmp), nqp::iterkey_s(tmp) )
                }
                else {
                    IterationEnd
                }
            }
            method push-all($target) {
                while $!hash-iter {
                    my \tmp = nqp::shift($!hash-iter);
                    $target.push(
                      Pair.new( nqp::iterval(tmp), nqp::iterkey_s(tmp) ));
                }
                IterationEnd
            }
        }.new(self))
    }
    multi method invert(Map:D:) {
        self.map: { (.value »=>» .key).cache.Slip }
    }

    multi method AT-KEY(Map:D: \key) is raw {
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
            if nqp::istype($x,Pair) {
                self.STORE_AT_KEY($x.key, $x.value)
            }
            elsif nqp::istype($x, Map) and !nqp::iscont($x) {
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

    proto method STORE_AT_KEY(|) is raw { * }
    multi method STORE_AT_KEY(Str \key, Mu \value) is raw {
        nqp::defined($!storage) ||
            nqp::bindattr(self, Map, '$!storage', nqp::hash());
        nqp::bindkey($!storage, nqp::unbox_s(key), value)
    }
    multi method STORE_AT_KEY(\key, Mu \value) is raw {
        nqp::defined($!storage) ||
            nqp::bindattr(self, Map, '$!storage', nqp::hash());
        nqp::bindkey($!storage, nqp::unbox_s(key.Str), value)
    }

    method Capture(Map:D:) {
        my $cap := nqp::create(Capture);
        nqp::bindattr($cap, Capture, '$!hash', $!storage);
        $cap
    }

    method FLATTENABLE_LIST() { nqp::list() }
    method FLATTENABLE_HASH() {
        nqp::defined($!storage) ||
            nqp::bindattr(self, Map, '$!storage', nqp::hash());
        $!storage
    }

    method fmt(Map: Cool $format = "%s\t\%s", $sep = "\n") {
        if nqp::p6box_i(nqp::sprintfdirectives( nqp::unbox_s($format.Stringy) )) == 1 {
            self.keys.fmt($format, $sep);
        }
        else {
            self.pairs.fmt($format, $sep);
        }
    }

    method hash() { self }
}

multi sub infix:<eqv>(Map:D $a, Map:D $b) {
    if +$a != +$b { return Bool::False }
    for $a.kv -> $k, $v {
        unless $b.EXISTS-KEY($k) && $b{$k} eqv $v {
            return Bool::False;
        }
    }
    Bool::True;
}

# vim: ft=perl6 expandtab sw=4
