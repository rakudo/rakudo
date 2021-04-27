my role Setty does QuantHash {
    has Rakudo::Internals::IterationSet $!elems; # key.WHICH => key

    method of() { Bool }

    # private method to create Set from iterator, check for laziness
    method !create-from-iterator(\type, \iterator --> Setty:D) {
        iterator.is-lazy
          ?? type.fail-iterator-cannot-be-lazy('coerce')
          !! nqp::create(type).SET-SELF(
               Rakudo::QuantHash.ADD-ITERATOR-TO-SET(
                 nqp::create(Rakudo::Internals::IterationSet),
                 iterator,
                 self.keyof
               )
             )
    }

    multi method new(Setty: --> Setty:D) { nqp::create(self) }
    multi method new(Setty: \value --> Setty:D) {
        nqp::if(
          nqp::istype(value,Iterable) && nqp::not_i(nqp::iscont(value)),
          self!create-from-iterator(self, value.iterator),
          nqp::stmts(
            nqp::bindkey(
              (my $elems := nqp::create(Rakudo::Internals::IterationSet)),
              value.WHICH,
              nqp::decont(value)
            ),
            nqp::create(self).SET-SELF($elems)
          )
        )
    }
    multi method new(Setty: **@args --> Setty:D) {
        self!create-from-iterator(self, @args.iterator)
    }

    method new-from-pairs(*@pairs --> Setty:D) {
        (my \iterator := @pairs.iterator).is-lazy
          ?? self.fail-iterator-cannot-be-lazy('coerce')
          !! nqp::create(self).SET-SELF(
               Rakudo::QuantHash.ADD-PAIRS-TO-SET(
                 nqp::create(Rakudo::Internals::IterationSet),
                 iterator,
                 self.keyof
               )
             )
    }

    method default(--> False) { }

    multi method keys(Setty:D:) {
        Seq.new(Rakudo::Iterator.Mappy-values($!elems))
    }

    method elems(Setty:D:) {
        nqp::istrue($!elems) && nqp::elems($!elems)
    }
    method total(Setty:D: --> Int:D) {
        nqp::istrue($!elems) && nqp::elems($!elems)
    }

    my class AntiPairs does Rakudo::Iterator::Mappy {
        method pull-one() {
            $!iter
              ?? Pair.new(True,nqp::iterval(nqp::shift($!iter)))
              !! IterationEnd
        }
    }
    multi method antipairs(Setty:D:) { Seq.new(AntiPairs.new($!elems)) }
    multi method minpairs(Setty:D:) { self.pairs }
    multi method maxpairs(Setty:D:) { self.pairs }
    multi method Bool(Setty:D: --> Bool:D) {
        nqp::hllbool($!elems ?? nqp::elems($!elems) !! 0)
    }

    method !HASHIFY(\type) {
        my \hash := Hash.^parameterize(type,Any).new;
        my \descriptor := nqp::getattr(hash,Hash,'$!descriptor');

        nqp::if(
          $!elems && nqp::elems($!elems),
          nqp::stmts(
            (my \storage := nqp::clone($!elems)),
            (my \iter := nqp::iterator(storage)),
            nqp::while(
              iter,
              nqp::bindkey(
                storage,
                nqp::iterkey_s(nqp::shift(iter)),
                Pair.new(
                  nqp::iterval(iter),
                  (nqp::p6scalarfromdesc(descriptor) = True)
                )
              )
            ),
            nqp::bindattr(hash,Map,'$!storage',storage)
          )
        );
        hash
    }
    multi method hash(Setty:D: --> Hash:D) { self!HASHIFY(Bool) }
    multi method Hash(Setty:D: --> Hash:D) { self!HASHIFY(Any) }

    method Map {
        nqp::if(
          $!elems && nqp::elems($!elems),
          nqp::stmts(
            (my \storage := nqp::hash),
            (my \iter := nqp::iterator($!elems)),
            nqp::while(
              iter,
              nqp::bindkey(
                storage,
                nqp::iterval(nqp::shift(iter)).Str,
                True
              )
            ),
            nqp::p6bindattrinvres(nqp::create(Map),Map,'$!storage',storage)
          ),
          nqp::create(Map)
        )
    }

    multi method ACCEPTS(Setty:U: \other --> Bool:D) {
        other.^does(self)
    }
    multi method ACCEPTS(Setty:D: Setty:D \other --> Bool:D) {
        self (==) other
    }
    multi method ACCEPTS(Setty:D: \other --> Bool:D) {
        self (==) other.Set
    }

    multi method Str(Setty:D $ : --> Str:D) {
        nqp::join(" ",Rakudo::QuantHash.RAW-VALUES-MAP(self, *.Str))
    }
    multi method gist(Setty:D $ : --> Str:D) {
        nqp::concat(
          nqp::concat(
            nqp::concat(self.^name,'('),
            nqp::join(" ",
              Rakudo::Sorting.MERGESORT-str(
                Rakudo::QuantHash.RAW-VALUES-MAP(self, *.gist)
              )
            )
          ),
          ')'
        )
    }
    multi method raku(Setty:D $ : --> Str:D) {
        nqp::if(
          nqp::eqaddr(self,set()),
          'set()',
          nqp::concat(
            nqp::concat(
              nqp::concat(self.^name,'.new('),
              nqp::join(",",Rakudo::QuantHash.RAW-VALUES-MAP(self, *.raku))
            ),
            ')'
          )
        )
    }

    proto method grab(|) {*}
    proto method grabpairs(|) {*}

    proto method pick(|) {*}
    multi method pick(Setty:D:) { self.roll }
    multi method pick(Setty:D: Callable:D $calculate) {
        self.pick( $calculate(self.elems) )
    }
    multi method pick(Setty:D: Whatever $) {
        self.pick(Inf)
    }

    my class PickN does Rakudo::QuantHash::Pairs {
        method pull-one() is raw {
            nqp::elems($!picked)
              ?? nqp::atkey($!elems,nqp::pop_s($!picked))
              !! IterationEnd
        }
    }
    multi method pick(Setty:D: $count) { Seq.new(PickN.new($!elems,$count)) }

    proto method pickpairs(|) {*}
    multi method pickpairs(Setty:D:) { Pair.new(self.roll,True) }
    multi method pickpairs(Setty:D: Callable:D $calculate) {
        self.pickpairs( $calculate(self.elems) )
    }
    multi method pickpairs(Setty:D: Whatever $) {
        self.pickpairs(Inf)
    }

    my class PickPairsN does Rakudo::QuantHash::Pairs {
        method pull-one() is raw {
            nqp::elems($!picked)
              ?? Pair.new(nqp::atkey($!elems,nqp::pop_s($!picked)),True)
              !! IterationEnd
        }
    }
    multi method pickpairs(Setty:D: \count) {
        Seq.new(PickPairsN.new($!elems,count))
    }

    proto method roll(|) {*}
    multi method roll(Setty:D:) {
        $!elems
          ??  nqp::iterval(Rakudo::QuantHash.ROLL($!elems))
          !! Nil
    }
    multi method roll(Setty:D: Callable:D $calculate) {
        self.roll($calculate(self.elems))
    }
    multi method roll(Setty:D: Whatever) {
        self.roll(Inf)
    }
    multi method roll(Setty:D: $count) {
        Seq.new(nqp::if(
          (my $todo = Rakudo::QuantHash.TODO($count))
            && $!elems
            && (my int $elems = nqp::elems($!elems)),
          nqp::stmts(
            (my $keys := Rakudo::QuantHash.RAW-KEYS(self)),
            nqp::if(
              $todo == Inf,
              Rakudo::Iterator.Callable(
                { nqp::atkey($!elems,nqp::atpos_s($keys,nqp::rand_n($elems))) },
                True
              ),
              Rakudo::Iterator.Callable( {
                  nqp::if(
                    $todo,
                    nqp::stmts(
                      --$todo,
                      nqp::atkey(
                        $!elems,
                        nqp::atpos_s($keys,nqp::rand_n($elems))
                      )
                    ),
                    IterationEnd
                  )
              } )
            )
          ),
          Rakudo::Iterator.Empty
        ))
    }

    multi method EXISTS-KEY(Setty:D: \k --> Bool:D) {
        nqp::hllbool($!elems ?? nqp::existskey($!elems,k.WHICH) !! 0)
    }

    multi method Bag(Setty:D:) {
        $!elems && nqp::elems($!elems)
          ?? nqp::create(Bag).SET-SELF(Rakudo::QuantHash.SET-BAGGIFY($!elems))
          !! bag()
    }
    multi method BagHash(Setty:D:) {
        $!elems && nqp::elems($!elems)
          ?? nqp::create(BagHash).SET-SELF(
               Rakudo::QuantHash.SET-BAGGIFY($!elems))
          !! nqp::create(BagHash)
    }
    multi method Mix(Setty:D:) {
        $!elems && nqp::elems($!elems)
          ?? nqp::create(Mix).SET-SELF(Rakudo::QuantHash.SET-BAGGIFY($!elems))
          !! mix()
    }
    multi method MixHash(Setty:D:) {
        $!elems && nqp::elems($!elems)
          ?? nqp::create(MixHash).SET-SELF(
               Rakudo::QuantHash.SET-BAGGIFY($!elems))
          !! nqp::create(MixHash)
    }

    method RAW-HASH() is raw is implementation-detail { $!elems }

    # TODO: WHICH will require the capability for >1 pointer in ObjAt
}

multi sub infix:<eqv>(Setty:D \a, Setty:D \b --> Bool:D) {
    nqp::hllbool(
      nqp::eqaddr(nqp::decont(a),nqp::decont(b))
        || (nqp::eqaddr(a.WHAT,b.WHAT) && a.ACCEPTS(b))
    )
}

# vim: expandtab shiftwidth=4
