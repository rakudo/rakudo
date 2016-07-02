my class X::Hash::Store::OddNumber { ... }

my class Map does Iterable does Associative { # declared in BOOTSTRAP
    # my class Map is Iterable is Cool {
    #   has Mu $!storage;

    multi method WHICH(Map:D:) {
        self.^name
          ~ '|'
          ~ self.keys.sort.map( { $_.WHICH ~ '(' ~ self.AT-KEY($_) ~ ')' } )
    }
    method new(*@args) {
        @args
          ?? nqp::create(self).STORE(@args)
          !! nqp::create(self)
    }

    multi method Map(Map:) { self }

    multi method Hash(Map:U:) { Hash }
    multi method Hash(Map:D:) {
        if nqp::defined($!storage) && nqp::elems($!storage) {
            my $hash       := nqp::create(Hash);
            my $storage    := nqp::bindattr($hash,Map,'$!storage',nqp::hash);
            my $descriptor := nqp::null;
            my $iter       := nqp::iterator(nqp::getattr(self,Map,'$!storage'));
            nqp::while(
              $iter,
              nqp::stmts(
                nqp::shift($iter),
                nqp::bindkey($storage,nqp::iterkey_s($iter),
                  nqp::p6scalarfromdesc($descriptor) =
                    nqp::decont(nqp::iterval($iter)))
              )
            );
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
                nqp::if(
                  $!iter,
                  nqp::stmts(
                    nqp::shift($!iter),
                    Pair.new(nqp::iterkey_s($!iter), nqp::iterval($!iter))
                  ),
                  IterationEnd
                )
            }
            method push-all($target) {
                nqp::while(
                  $!iter,
                  nqp::stmts(  # doesn't sink
                     nqp::shift($!iter),
                     $target.push(
                       Pair.new(nqp::iterkey_s($!iter), nqp::iterval($!iter)))
                  )
                );
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
                nqp::while(
                  $!iter,
                  $target.push(nqp::iterkey_s(nqp::shift($!iter)))
                );
                IterationEnd
            }
        }.new(self))
    }
    multi method kv(Map:D:) {
        Seq.new(class :: does Rakudo::Internals::MappyIterator {
            has int $!on-value;

            method pull-one() is raw {
                nqp::if(
                  $!on-value,
                  nqp::stmts(
                    ($!on-value = 0),
                    nqp::iterval($!iter)
                  ),
                  nqp::if(
                    $!iter,
                    nqp::stmts(
                      ($!on-value = 1),
                      nqp::iterkey_s(nqp::shift($!iter))
                    ),
                    IterationEnd
                  )
                )
            }
            method push-all($target) {
                nqp::while(  # doesn't sink
                  $!iter,
                  nqp::stmts(
                    $target.push(nqp::iterkey_s(nqp::shift($!iter))),
                    $target.push(nqp::iterval($!iter))
                  )
                );
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
                nqp::while(  # doesn't sink
                  $!iter,
                  $target.push(nqp::iterval(nqp::shift($!iter)))
                );
                IterationEnd
            }
        }.new(self))
    }
    multi method antipairs(Map:D:) {
        Seq.new(class :: does Rakudo::Internals::MappyIterator {
            method pull-one() {
                nqp::if(
                  $!iter,
                  nqp::stmts(
                    nqp::shift($!iter),
                    Pair.new( nqp::iterval($!iter), nqp::iterkey_s($!iter) )
                  ),
                  IterationEnd
                );
            }
            method push-all($target) {
                nqp::while(
                  $!iter,
                  nqp::stmts(  # doesn't sink
                    nqp::shift($!iter),
                    $target.push(
                      Pair.new( nqp::iterval($!iter), nqp::iterkey_s($!iter) ))
                  )
                );
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
        method !STORE_LIST(\x --> Nil) {
            self.STORE_AT_KEY(.key,.value) for x.list;
        }

        $!storage := nqp::hash();
        my $iter  := to_store.iterator;
        my Mu $x;
        my Mu $y;

        nqp::istype($x,Pair)
          ?? self.STORE_AT_KEY($x.key, $x.value)
          !! nqp::istype($x, Map) && !nqp::iscont($x)
            ?? self!STORE_LIST($x)
            !! (($y := $iter.pull-one) =:= IterationEnd)
              ?? nqp::istype($x,Failure)
                ?? $x.throw
                !! X::Hash::Store::OddNumber.new.throw
              !! self.STORE_AT_KEY($x,$y)
                until ($x := $iter.pull-one) =:= IterationEnd;
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
        nqp::defined($!storage)
          ?? nqp::p6bindattrinvres(
               nqp::create(Capture),Capture,'$!hash',$!storage)
          !! nqp::create(Capture)
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
    method clone(Map:D: *%) is raw {
        nqp::p6bindattrinvres(
          nqp::create(self),Map,'$!storage',nqp::clone($!storage))
    }
}

multi sub infix:<eqv>(Map:D \a, Map:D \b) {
    nqp::if(
      nqp::eqaddr(a,b),
      True,
      nqp::if(
        (nqp::eqaddr(a.WHAT,b.WHAT) && (my int $elems = a.elems) == b.elems),
        nqp::if(
          nqp::iseq_i($elems,0),
          True,
          nqp::stmts(
            (my $amap := nqp::getattr(nqp::decont(a),Map,'$!storage')),
            (my $bmap := nqp::getattr(nqp::decont(b),Map,'$!storage')),
            (my $iter := nqp::iterator($amap)),
            nqp::while(
              $iter,
              nqp::unless(
                (nqp::existskey(
                  $bmap,my str $key = nqp::iterkey_s(nqp::shift($iter))
                ) && nqp::atkey($amap,$key) eqv nqp::atkey($bmap,$key)),
                return False
              )
            ),
            True
          )
        ),
        False
      )
    )
}

# vim: ft=perl6 expandtab sw=4
