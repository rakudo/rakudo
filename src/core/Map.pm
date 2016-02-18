my class X::Hash::Store::OddNumber { ... }

my class Map does Iterable does Associative { # declared in BOOTSTRAP
    # my class Map is Iterable is Cool {
    #   has Mu $!storage;

    method new(*@args) {
        @args
          ?? nqp::create(self).STORE(@args)
          !! nqp::create(self)
    }

    multi method Hash(Map:U:) { Hash }
    multi method Hash(Map:D:) {
        if nqp::defined($!storage) && nqp::elems($!storage) {
            my $hash       := nqp::create(Hash);
            my $storage    := nqp::bindattr($hash,Map,'$!storage',nqp::hash);
            my $descriptor := nqp::null;
            my $iter       := nqp::iterator(nqp::getattr(self,Map,'$!storage'));
            while $iter {
                my $tmp := nqp::shift($iter);
                nqp::bindkey($storage,nqp::iterkey_s($tmp),
                  nqp::p6scalarfromdesc($descriptor) =
                    nqp::decont(nqp::iterval($tmp)));
            }
            $hash
        }
        else {
            nqp::create(Hash)
        }
    }

    multi method Bool(Map:D:) {
        nqp::p6bool(nqp::defined($!storage) && nqp::elems($!storage));
    }
    method elems(Map:D:) {
        nqp::p6box_i(nqp::defined($!storage) && nqp::elems($!storage));
    }
    multi method Int(Map:D:)     { self.elems }
    multi method Numeric(Map:D:) { self.elems }
    multi method Str(Map:D:)     { self.pairs.sort.join("\n") }

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
          ~ '.new(('
          ~ self.pairs.sort.map({.perl}).join(',')
          ~ '))';
    }

    method iterator(Map:) { self.pairs.iterator }
    method list(Map:) { self.pairs.cache }

    multi method pairs(Map:D:) {
        Seq.new(class :: does Rakudo::Internals::MappyIterator {
            method pull-one() {
                if $!iter {
                    my \tmp = nqp::shift($!iter);
                    Pair.new(nqp::iterkey_s(tmp), nqp::iterval(tmp))
                }
                else {
                    IterationEnd
                }
            }
            method push-all($target) {
                my $no-sink;
                while $!iter {
                    my \tmp = nqp::shift($!iter);
                    $no-sink := $target.push(
                      Pair.new(nqp::iterkey_s(tmp), nqp::iterval(tmp)));
                }
                IterationEnd
            }
        }.new(self))
    }
    multi method keys(Map:D:) {
        Seq.new(class :: does Rakudo::Internals::MappyIterator {
            method pull-one() {
                $!iter
                    ?? nqp::iterkey_s(nqp::shift($!iter))
                    !! IterationEnd
            }
            method push-all($target) {
                my $no-sink;
                $no-sink :=
                  $target.push(nqp::iterkey_s(nqp::shift($!iter)))
                    while $!iter;
                IterationEnd
            }
        }.new(self))
    }
    multi method kv(Map:D:) {
        Seq.new(class :: does Rakudo::Internals::MappyIterator {
            has int $!on-value;

            method pull-one() is raw {
                if $!on-value {
                    $!on-value = 0;
                    nqp::iterval($!iter)
                }
                elsif $!iter {
                    my \tmp = nqp::shift($!iter);
                    $!on-value = 1;
                    nqp::iterkey_s(tmp)
                }
                else {
                    IterationEnd
                }
            }
            method push-all($target) {
                my $no-sink;
                while $!iter {
                    my \tmp = nqp::shift($!iter);
                    $no-sink := $target.push(nqp::iterkey_s(tmp));
                    $no-sink := $target.push(nqp::iterval(tmp));
                }
                IterationEnd
            }
        }.new(self))
    }
    multi method values(Map:D:) {
        Seq.new(class :: does Rakudo::Internals::MappyIterator {
            method pull-one() is raw {
                $!iter
                    ?? nqp::iterval(nqp::shift($!iter))
                    !! IterationEnd
            }
            method push-all($target) {
                my $no-sink;
                $no-sink := $target.push(nqp::iterval(nqp::shift($!iter)))
                  while $!iter;
                IterationEnd
            }
        }.new(self))
    }
    multi method antipairs(Map:D:) {
        Seq.new(class :: does Rakudo::Internals::MappyIterator {
            method pull-one() {
                if $!iter {
                    my \tmp = nqp::shift($!iter);
                    Pair.new( nqp::iterval(tmp), nqp::iterkey_s(tmp) )
                }
                else {
                    IterationEnd
                }
            }
            method push-all($target) {
                my $no-sink;
                while $!iter {
                    my \tmp = nqp::shift($!iter);
                    $no-sink := $target.push(
                      Pair.new( nqp::iterval(tmp), nqp::iterkey_s(tmp) ));
                }
                IterationEnd
            }
        }.new(self))
    }
    multi method invert(Map:D:) {
        self.map: { (.value »=>» .key).cache.Slip }
    }

    multi method AT-KEY(Map:D: Str:D \key) is raw {
        nqp::defined($!storage)
          ?? nqp::ifnull(nqp::atkey($!storage,nqp::unbox_s(key)),Nil)
          !! Nil
    }
    multi method AT-KEY(Map:D: \key) is raw {
        nqp::defined($!storage)
          ?? nqp::ifnull(nqp::atkey($!storage,nqp::unbox_s(key.Str)),Nil)
          !! Nil
    }

    method STORE(\to_store) {
        my \iter = to_store.iterator;
        $!storage := nqp::hash();
        until (my Mu $x := iter.pull-one) =:= IterationEnd {
            if nqp::istype($x,Pair) {
                self.STORE_AT_KEY($x.key, $x.value)
            }
            elsif nqp::istype($x, Map) and !nqp::iscont($x) {
                for $x.list { self.STORE_AT_KEY(.key, .value) }
            }
            elsif !((my Mu $y := iter.pull-one) =:= IterationEnd) {
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

    proto method STORE_AT_KEY(|) { * }
    multi method STORE_AT_KEY(Str:D \key, Mu \value --> Nil) {
        $!storage := nqp::hash unless nqp::defined($!storage);
        nqp::bindkey($!storage, nqp::unbox_s(key), value)
    }
    multi method STORE_AT_KEY(\key, Mu \value --> Nil) {
        $!storage := nqp::hash unless nqp::defined($!storage);
        nqp::bindkey($!storage, nqp::unbox_s(key.Str), value)
    }

    method Capture(Map:D:) {
        if nqp::defined($!storage) {
            my $cap := nqp::create(Capture);
            nqp::bindattr($cap,Capture,'$!hash',$!storage);
            $cap
        }
        else {
            nqp::create(Capture)
        }
    }

    method FLATTENABLE_LIST() { nqp::list() }
    method FLATTENABLE_HASH() {
        nqp::defined($!storage)
          ?? $!storage
          !! nqp::bindattr(self,Map,'$!storage',nqp::hash)
    }

    method fmt(Map: Cool $format = "%s\t\%s", $sep = "\n") {
        nqp::iseq_i(nqp::sprintfdirectives( nqp::unbox_s($format.Stringy)),1)
          ?? self.keys.fmt($format, $sep)
          !! self.pairs.fmt($format, $sep)
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
