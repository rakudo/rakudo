my class X::Hash::Store::OddNumber { ... }

my class Map does Iterable does Associative { # declared in BOOTSTRAP
    # my class Map is Iterable is Cool
    #   has Mu $!storage;

    # Calling self.new for the arguments case ensures that the right
    # descriptor will be added for typed hashes.
    multi method new(Map:        --> Map:D) {
        nqp::create(self)
    }
    multi method new(Map: *@args --> Map:D) {
        self.new.STORE(@args, :INITIALIZE)
    }

    multi method contains(Map:D: \needle) {
        my $name := self.^name;
        warn "Applying '.contains' to a $name will look at its .Str representation.  Did you mean '$name\{needle}:exists'?".naive-word-wrapper;
        self.Str.contains(needle)
    }

    multi method index(Map:D: \needle) {
        my $name := self.^name;
        warn "Applying '.index' to a $name will look at its .Str representation.  Did you mean '$name\{needle}:exists'?".naive-word-wrapper;
        self.Str.index(needle)
    }

    multi method Map(Map:) { self }

    multi method Hash(Map:U:) { Hash }
    multi method Hash(Map:D: --> Hash:D) {
        if nqp::iterator($!storage) -> \iter {
            my \hash       := nqp::create(Hash);
            my \storage    := nqp::bindattr(hash,Map,'$!storage',nqp::hash);
            my \descriptor := BEGIN nqp::getcurhllsym('default_cont_spec');
            nqp::while(
              iter,
              nqp::bindkey(
                storage,
                nqp::iterkey_s(nqp::shift(iter)),
                nqp::p6scalarwithvalue(
                  descriptor, nqp::decont(nqp::iterval(iter)))
              )
            );
            hash
        }
        else {
            nqp::create(Hash)
        }
    }

    multi method Bool(Map:D: --> Bool:D) {
        nqp::hllbool(nqp::elems($!storage));
    }
    method elems(Map:D:) { nqp::elems($!storage) }
    multi method Int(Map:D:     --> Int:D) { self.elems }
    multi method Numeric(Map:D: --> Int:D) { self.elems }

    multi method Str(Map:D: --> Str:D) { self.sort.join("\n") }

    method IterationBuffer(--> IterationBuffer:D) {
        my \buffer := nqp::create(IterationBuffer);
        nqp::if(
          nqp::elems($!storage),
          nqp::stmts(
            (my \iterator := nqp::iterator($!storage)),
            nqp::setelems(buffer,nqp::elems($!storage)),
            (my int $i = -1),
            nqp::while(
              iterator,
              nqp::bindpos(buffer,($i = nqp::add_i($i,1)),
                Pair.new(
                  nqp::iterkey_s(nqp::shift(iterator)),
                  nqp::iterval(iterator)
                )
              )
            )
          )
        );
        buffer
    }

    method List(--> List:D) { self.IterationBuffer.List }

    multi method head(Map:D:) {
        nqp::elems($!storage)
          ?? Pair.new(
               nqp::iterkey_s(
                 nqp::shift(my \iterator := nqp::iterator($!storage))
               ),
               nqp::iterval(iterator)
             )
          !! Nil
    }

    # Produce a native str array with all the keys
    method !keys-as-str() {
        my $keys := nqp::list_s;
        nqp::if(
          ($!storage && my \iter := nqp::iterator($!storage)),
          nqp::while(
            iter,
            nqp::push_s($keys,nqp::iterkey_s(nqp::shift(iter)))
          )
        );
        $keys
    }

    # Iterator over a native string array holding the keys and producing
    # Pairs.
    my class Iterate-keys does Iterator {
        has $!map  is built(:bind);
        has $!keys is built(:bind);
        has int $!i = -1;
        method pull-one() {
            nqp::if(
              nqp::islt_i(($!i = nqp::add_i($!i,1)),nqp::elems($!keys)),
              nqp::stmts(
                (my \key := nqp::atpos_s($!keys,$!i)),
                Pair.new(key,nqp::atkey($!map,key))
              ),
              IterationEnd
            )
        }
        method push-all($target --> IterationEnd) {
            my \map  := $!map;
            my \keys := $!keys;
            my int $i = $!i;
            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),nqp::elems(keys)),
              nqp::stmts(
                (my \key := nqp::atpos_s(keys,$i)),
                $target.push(Pair.new(key,nqp::atkey(map,key)))
              )
            );
            $!i = $i;
        }
    }

    multi method sort(Map:D: --> Seq:D) {
        Seq.new(
          Iterate-keys.new(
            map  => self,
            keys => Rakudo::Sorting.MERGESORT-str(self!keys-as-str)
          )
        )
    }

    multi method ACCEPTS(Map:D: Any $topic --> Bool:D) {
        self.EXISTS-KEY($topic.any);
    }

    multi method ACCEPTS(Map:D: Cool:D $topic --> Bool:D) {
        self.EXISTS-KEY($topic);
    }

    multi method ACCEPTS(Map:D: Positional $topic --> Bool:D) {
        self.EXISTS-KEY($topic.any);
    }

    multi method ACCEPTS(Map:D: Regex $topic --> Bool:D) {
        so self.keys.any.match($topic);
    }

    multi method ACCEPTS(Map:D: Map:D \m --> Bool:D) {
        try {self eqv m} // False;
    }

    multi method EXISTS-KEY(Map:D: Str:D \key --> Bool:D) {
        nqp::hllbool(nqp::existskey($!storage,key))
    }
    multi method EXISTS-KEY(Map:D: \key --> Bool:D) {
        nqp::hllbool(nqp::existskey($!storage,key.Str))
    }

    multi method gist(Map:D: --> Str:D) {
        self.^name
          ~ '.new(('
          ~ self.sort.head(100).map(*.gist).join(', ')
          ~ (', ...' if self.elems > 100)
          ~ '))'
    }

    multi method raku(Map:D \SELF: --> Str:D) {
        my $p := nqp::elems($!storage)
          ?? self.^name ~ '.new((' ~ self.sort.map(*.raku).join(',') ~ '))'
          !! self.^name ~ '.new';
        nqp::iscont(SELF) ?? '$(' ~ $p ~ ')' !! $p
    }

    my class Iterate does Rakudo::Iterator::Mappy {
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
        method push-all(\target --> IterationEnd) {
            nqp::while(
              $!iter,
              nqp::stmts(  # doesn't sink
                 nqp::shift($!iter),
                 target.push(
                   Pair.new(nqp::iterkey_s($!iter), nqp::iterval($!iter)))
              )
            )
        }
    }

    multi method iterator(Map:D: --> Iterator:D) { Iterate.new(self) }
    multi method list(Map:D: --> List:D) { self.List }

    multi method pairs(Map:D: --> Seq:D) {
        Seq.new(self.iterator)
    }
    multi method keys(Map:D: --> Seq:D) {
        Seq.new(Rakudo::Iterator.Mappy-keys(self))
    }
    multi method values(Map:D: --> Seq:D) {
        Seq.new(Rakudo::Iterator.Mappy-values(self))
    }

    my class KV does Rakudo::Iterator::Mappy-kv-from-pairs {
        method pull-one() is raw {
            nqp::if(
              $!on,
              nqp::stmts(
                ($!on= 0),
                nqp::iterval($!iter)
              ),
              nqp::if(
                $!iter,
                nqp::stmts(
                  ($!on= 1),
                  nqp::iterkey_s(nqp::shift($!iter))
                ),
                IterationEnd
              )
            )
        }
        method push-all(\target --> IterationEnd) {
            nqp::while(  # doesn't sink
              $!iter,
              nqp::stmts(
                target.push(nqp::iterkey_s(nqp::shift($!iter))),
                target.push(nqp::iterval($!iter))
              )
            )
        }
    }
    multi method kv(Map:D: --> Seq:D) { Seq.new(KV.new(self)) }

    my class AntiPairs does Rakudo::Iterator::Mappy {
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
        method push-all(\target --> IterationEnd) {
            nqp::while(
              $!iter,
              nqp::stmts(  # doesn't sink
                nqp::shift($!iter),
                target.push(
                  Pair.new( nqp::iterval($!iter), nqp::iterkey_s($!iter) ))
              )
            )
        }
    }
    multi method antipairs(Map:D: --> Seq:D) { Seq.new(AntiPairs.new(self)) }

    multi method invert(Map:D: --> Seq:D) {
        Seq.new(Rakudo::Iterator.Invert(self.iterator))
    }

    multi method AT-KEY(Map:D: Str:D \key) is raw {
        nqp::ifnull(nqp::atkey($!storage,nqp::unbox_s(key)),Nil)
    }
    multi method AT-KEY(Map:D: \key) is raw {
        nqp::ifnull(nqp::atkey($!storage,nqp::unbox_s(key.Str)),Nil)
    }

    multi method ASSIGN-KEY(Map:D: \key, Mu \new) {
        nqp::isnull(my \old := nqp::atkey($!storage,key.Str))
          ?? die("Cannot add key '{key}' to an immutable {self.^name}")
          !! nqp::iscont(old)
            ?? (old = new)
            !! die("Cannot change key '{key}' in an immutable {self.^name}")
    }

    # Directly copy from the other Map's internals.
    method !STORE_MAP_FROM_MAP_DECONT(\map --> Map:D) {
        nqp::if(
          nqp::elems(my \other := nqp::getattr(map,Map,'$!storage')),
          nqp::stmts(
            (my \iter := nqp::iterator(other)),
            nqp::while(
              iter,
              nqp::bindkey(
                $!storage,
                nqp::iterkey_s(nqp::shift(iter)),
                nqp::decont(nqp::iterval(iter))
              )
            )
          )
        );
        self
    }
    method !STORE_MAP_FROM_MAP(\map --> Map:D) {
        nqp::if(
          nqp::elems(my \other := nqp::getattr(map,Map,'$!storage')),
          nqp::stmts(
            (my \iter := nqp::iterator(other)),
            nqp::while(
              iter,
              nqp::bindkey(
                $!storage,
                nqp::iterkey_s(nqp::shift(iter)),
                nqp::iterval(iter)
              )
            )
          )
        );
        self
    }

    # Directly copy from the Object Hash's internals, but pay respect to the
    # fact that we're only interested in the values (which contain a Pair with
    # the object key and a value that we need to decontainerize.
    method !STORE_MAP_FROM_OBJECT_HASH_DECONT(\map --> Map:D) {
        nqp::if(
          nqp::elems(my \other := nqp::getattr(map,Map,'$!storage')),
          nqp::stmts(
            (my \iter := nqp::iterator(other)),
            nqp::while(
              iter,
              nqp::bindkey(
                $!storage,
                nqp::getattr(
                  (my Mu \pair := nqp::iterval(nqp::shift(iter))),
                  Pair, '$!key'
                ).Str,
                nqp::decont(nqp::getattr(pair,Pair,'$!value'))
              )
            )
          )
        );
        self
    }
    method !STORE_MAP_FROM_OBJECT_HASH(\map --> Map:D) {
        nqp::if(
          nqp::elems(my \other := nqp::getattr(map,Map,'$!storage')),
          nqp::stmts(
            (my \iter := nqp::iterator(other)),
            nqp::while(
              iter,
              nqp::bindkey(
                $!storage,
                nqp::getattr(
                  (my Mu \pair := nqp::iterval(nqp::shift(iter))),
                  Pair, '$!key'
                ).Str,
                nqp::getattr(pair,Pair,'$!value')
              )
            )
          )
        );
        self
    }

    # Copy the contents of a Mappy thing that's not in a container.
    method !STORE_MAP_DECONT(\map --> Map:D) {
        nqp::istype(map,Hash::Object)
          ?? self!STORE_MAP_FROM_OBJECT_HASH_DECONT(map)
          !! self!STORE_MAP_FROM_MAP_DECONT(map)
    }
    method !STORE_MAP(\map --> Map:D) {
        nqp::istype(map,Hash::Object)
          ?? self!STORE_MAP_FROM_OBJECT_HASH(map)
          !! self!STORE_MAP_FROM_MAP(map)
    }

    # Store the contents of an iterator into the Map
    method !STORE_MAP_FROM_ITERATOR_DECONT(\iter --> Map:D) is raw {
        nqp::until(
          nqp::eqaddr((my Mu $x := iter.pull-one),IterationEnd),
          nqp::if(
            nqp::istype($x,Pair),
            nqp::bindkey(
              $!storage,
              nqp::getattr(nqp::decont($x),Pair,'$!key').Str,
              nqp::decont(nqp::getattr(nqp::decont($x),Pair,'$!value'))
            ),
            nqp::if(
              (nqp::istype($x,Map) && nqp::not_i(nqp::iscont($x))),
              self!STORE_MAP_DECONT($x),
              nqp::if(
                nqp::eqaddr((my Mu $y := iter.pull-one),IterationEnd),
                nqp::if(
                  nqp::istype($x,Failure),
                  $x.throw,
                  X::Hash::Store::OddNumber.new(
                    found => self.elems * 2 + 1,
                    last  => $x
                  ).throw
                ),
                nqp::bindkey($!storage,$x.Str,nqp::decont($y))
              )
            )
          )
        );
        self
    }
    method !STORE_MAP_FROM_ITERATOR(\iter --> Map:D) is raw {
        nqp::until(
          nqp::eqaddr((my Mu $x := iter.pull-one),IterationEnd),
          nqp::if(
            nqp::istype($x,Pair),
            nqp::bindkey(
              $!storage,
              nqp::getattr(nqp::decont($x),Pair,'$!key').Str,
              nqp::getattr(nqp::decont($x),Pair,'$!value')
            ),
            nqp::if(
              (nqp::istype($x,Map) && nqp::not_i(nqp::iscont($x))),
              self!STORE_MAP($x),
              nqp::if(
                nqp::eqaddr((my Mu $y := iter.pull-one),IterationEnd),
                nqp::if(
                  nqp::istype($x,Failure),
                  $x.throw,
                  X::Hash::Store::OddNumber.new(
                    found => self.elems * 2 + 1,
                    last  => $x
                  ).throw
                ),
                nqp::bindkey($!storage,$x.Str,$y)
              )
            )
          )
        );
        self
    }

    proto method STORE(Map:D: |) {*}
    multi method STORE(Map:D: Map:D \map, :INITIALIZE($)!, :DECONT($)! --> Map:D) {
        nqp::if(
          nqp::istype(map,Hash::Object),
          self!STORE_MAP_FROM_OBJECT_HASH_DECONT(map),
          nqp::if(
            nqp::elems(my \other := nqp::getattr(map,Map,'$!storage')),
            nqp::if(
              nqp::eqaddr(map.WHAT,Map),
              nqp::p6bindattrinvres(self,Map,'$!storage',other),
              self.STORE(map.iterator, :INITIALIZE, :DECONT)
            ),
            self                        # nothing to do
          )
        )
    }
    multi method STORE(Map:D: Map:D \map, :INITIALIZE($)! --> Map:D) {
        nqp::if(
          nqp::istype(map,Hash::Object),
          self!STORE_MAP_FROM_OBJECT_HASH(map),
          nqp::if(
            nqp::elems(my \other := nqp::getattr(map,Map,'$!storage')),
            nqp::if(
              nqp::eqaddr(map.WHAT,Map),
              nqp::p6bindattrinvres(self,Map,'$!storage',other),
              nqp::p6bindattrinvres(self,Map,'$!storage',nqp::clone(other))
            ),
            self                        # nothing to do
          )
        )
    }
    multi method STORE(Map:D: Iterator:D \iter, :INITIALIZE($)!, :DECONT($)! --> Map:D) {
        self!STORE_MAP_FROM_ITERATOR_DECONT(iter)
    }
    multi method STORE(Map:D: Iterator:D \iter, :INITIALIZE($)! --> Map:D) {
        self!STORE_MAP_FROM_ITERATOR(iter)
    }
    multi method STORE(Map:D: \to_store, :INITIALIZE($)!, :DECONT($)! --> Map:D) {
        self!STORE_MAP_FROM_ITERATOR_DECONT(to_store.iterator)
    }
    multi method STORE(Map:D: \to_store, :INITIALIZE($)! --> Map:D) {
        self!STORE_MAP_FROM_ITERATOR(to_store.iterator)
    }
    multi method STORE(Map:D: \keys, \values, :INITIALIZE($)! --> Map:D) {
        my \iterkeys   := keys.iterator;
        my \itervalues := values.iterator;
        my \storage    := $!storage := nqp::hash;
        nqp::until(
          nqp::eqaddr((my \key := iterkeys.pull-one),IterationEnd),
          nqp::bindkey(
            storage,
            nqp::if(nqp::istype(key,Str),key,key.Str),
            itervalues.pull-one
          )
        );
        self
    }
    multi method STORE(Map:D: |) { X::Assignment::RO.new(value => self).throw }

    method Capture(Map:D:) {
        nqp::p6bindattrinvres(nqp::create(Capture),Capture,'%!hash',$!storage)
    }

    method FLATTENABLE_LIST() is implementation-detail { nqp::list() }
    method FLATTENABLE_HASH() is implementation-detail { $!storage }

    method fmt(Map: Cool $format = "%s\t\%s", $sep = "\n" --> Str:D) {
        nqp::iseq_i(nqp::sprintfdirectives( nqp::unbox_s($format.Stringy)),1)
          ?? self.keys.fmt($format, $sep)
          !! self.pairs.fmt($format, $sep)
    }

    method hash() { self }
    method clone(Map:D:) { self }

    multi method roll(Map:D:) {
        nqp::if(
          $!storage && nqp::elems($!storage),
          nqp::stmts(
            (my int $i =
              nqp::add_i(nqp::floor_n(nqp::rand_n(nqp::elems($!storage))),1)),
            (my \iter := nqp::iterator($!storage)),
            nqp::while(
              nqp::shift(iter) && ($i = nqp::sub_i($i,1)),
              nqp::null
            ),
            Pair.new(nqp::iterkey_s(iter),nqp::iterval(iter))
          ),
          Nil
        )
    }
    multi method roll(Map:D: Callable:D $calculate) {
        self.roll( $calculate(self.elems) )
    }
    multi method roll(Map:D: Whatever $) { self.roll(Inf) }
    my class RollN does Iterator {
        has $!storage;
        has $!keys;
        has $!pairs;
        has $!count;

        method !SET-SELF(\hash,\count) {
            $!storage := nqp::getattr(hash,Map,'$!storage');
            $!count = count;
            my int $i = nqp::elems($!storage);
            my \iter := nqp::iterator($!storage);
            $!keys := nqp::setelems(nqp::list_s,$i);
            $!pairs := nqp::setelems(nqp::list,$i);

            nqp::while(
              nqp::isge_i(($i = nqp::sub_i($i,1)),0),
              nqp::bindpos_s($!keys,$i,
                nqp::iterkey_s(nqp::shift(iter)))
            );
            self
        }
        method new(\h,\c) { nqp::create(self)!SET-SELF(h,c) }
        method pull-one() {
            nqp::if(
              $!count,
              nqp::stmts(
                --$!count,  # must be HLL to handle Inf
                nqp::ifnull(
                  nqp::atpos(
                    $!pairs,
                    (my int $i =
                      nqp::floor_n(nqp::rand_n(nqp::elems($!keys))))
                  ),
                  nqp::bindpos($!pairs,$i,
                    Pair.new(
                      nqp::atpos_s($!keys,$i),
                      nqp::atkey($!storage,nqp::atpos_s($!keys,$i))
                    )
                  )
                )
              ),
              IterationEnd
            )
        }
        method is-lazy() { $!count == Inf }
        method is-deterministic(--> False) { }
    }
    multi method roll(Map:D: $count) {
        Seq.new(
          $!storage && nqp::elems($!storage) && $count > 0
            ?? RollN.new(self,$count)
            !! Rakudo::Iterator.Empty
        )
    }

    multi method pick(Map:D:) { self.roll }

    multi method Set(Map:D: --> Set:D) {
        nqp::create(Set).SET-SELF(Rakudo::QuantHash.COERCE-MAP-TO-SET(self))
    }
    multi method SetHash(Map:D: --> SetHash:D) {
        nqp::create(SetHash).SET-SELF(Rakudo::QuantHash.COERCE-MAP-TO-SET(self))
    }
    multi method Bag(Map:D: --> Bag:D) {
        nqp::create(Bag).SET-SELF(Rakudo::QuantHash.COERCE-MAP-TO-BAG(self))
    }
    multi method BagHash(Map:D: --> BagHash:D) {
        nqp::create(BagHash).SET-SELF(Rakudo::QuantHash.COERCE-MAP-TO-BAG(self))
    }
    multi method Mix(Map:D: --> Mix:D)     {
        nqp::create(Mix).SET-SELF(Rakudo::QuantHash.COERCE-MAP-TO-MIX(self))
    }
    multi method MixHash(Map:D: --> MixHash:D)     {
        nqp::create(MixHash).SET-SELF(Rakudo::QuantHash.COERCE-MAP-TO-MIX(self))
    }
}

multi sub infix:<eqv>(Map:D \a, Map:D \b --> Bool:D) {

    class NotEQV { }

    nqp::hllbool(
      nqp::unless(
        nqp::eqaddr(nqp::decont(a),nqp::decont(b)),
        nqp::if(                                 # not comparing with self
          nqp::eqaddr(a.WHAT,b.WHAT),
          nqp::if(                               # same types
            (my \amap := nqp::getattr(nqp::decont(a),Map,'$!storage'))
              && (my int $elems = nqp::elems(amap)),
            nqp::if(                             # elems on left
              (my \bmap := nqp::getattr(nqp::decont(b),Map,'$!storage'))
                && nqp::iseq_i($elems,nqp::elems(bmap)),
              nqp::stmts(                        # same elems on right
                (my \iter := nqp::iterator(amap)),
                nqp::while(
                  iter && infix:<eqv>(
                    nqp::iterval(nqp::shift(iter)),
                    nqp::ifnull(nqp::atkey(bmap,nqp::iterkey_s(iter)),NotEQV)
                  ),
                  ($elems = nqp::sub_i($elems,1))
                ),
                nqp::not_i($elems)               # ok if none left
              ),
              0
            ),
            nqp::isfalse(                        # nothing on left
              (my \map := nqp::getattr(nqp::decont(b),Map,'$!storage'))
                && nqp::elems(map)               # something on right: fail
            )
          )
        )
      )
    )
}

# vim: expandtab shiftwidth=4
