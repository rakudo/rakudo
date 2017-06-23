my role Setty does QuantHash {
    has $!elems; # key.WHICH => key

    # helper sub to create Set from iterator, check for laziness
    sub create-from-iterator(\type, \iterator --> Setty:D) {
        nqp::if(
          iterator.is-lazy,
          Failure.new(X::Cannot::Lazy.new(:action<coerce>,:what(type.^name))),
          nqp::create(type).SET-SELF(
            Rakudo::QuantHash.ADD-ITERATOR-TO-SET(
              nqp::create(Rakudo::Internals::IterationSet), iterator
            )
          )
        )
    }

    multi method new(Setty: --> Setty:D) { nqp::create(self) }
    multi method new(Setty: \value --> Setty:D) {
        nqp::if(
          nqp::istype(value,Iterable) && nqp::not_i(nqp::iscont(value)),
          create-from-iterator(self, value.iterator),
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
        create-from-iterator(self, @args.iterator)
    }

    method new-from-pairs(*@pairs --> Setty:D) {
        nqp::if(
          (my $iterator := @pairs.iterator).is-lazy,
          Failure.new(X::Cannot::Lazy.new(:action<coerce>,:what(self.^name))),
          nqp::create(self).SET-SELF(
            Rakudo::QuantHash.ADD-PAIRS-TO-SET(
              nqp::create(Rakudo::Internals::IterationSet), $iterator
            )
          )
        )
    }

    method default(--> False) { }

    multi method keys(Setty:D:) {
        Seq.new(Rakudo::Iterator.Mappy-values(self.hll_hash))
    }

    method elems(Setty:D: --> Int:D) {
        nqp::istrue($!elems) && nqp::elems($!elems)
    }
    method total(Setty:D: --> Int:D) {
        nqp::istrue($!elems) && nqp::elems($!elems)
    }
    multi method antipairs(Setty:D:) {
        Seq.new(class :: does Rakudo::Iterator::Mappy {
            method pull-one() {
              nqp::if(
                $!iter,
                Pair.new(True,nqp::iterval(nqp::shift($!iter))),
                IterationEnd
              )
            }
        }.new(self.hll_hash))
    }
    multi method minpairs(Setty:D:) { self.pairs }
    multi method maxpairs(Setty:D:) { self.pairs }
    multi method Bool(Setty:D:) {
        nqp::p6bool(nqp::istrue($!elems) && nqp::elems($!elems))
    }

    method HASHIFY(\type) {
        nqp::stmts(
          (my $hash := Hash.^parameterize(type,Any).new),
          (my $descriptor := nqp::getattr($hash,Hash,'$!descriptor')),
          nqp::if(
            $!elems && nqp::elems($!elems),
            nqp::stmts(
              (my $storage := nqp::clone($!elems)),
              (my $iter := nqp::iterator($storage)),
              nqp::while(
                $iter,
                nqp::bindkey(
                  $storage,
                  nqp::iterkey_s(nqp::shift($iter)),
                  Pair.new(
                    nqp::iterval($iter),
                    (nqp::p6scalarfromdesc($descriptor) = True)
                  )
                )
              ),
              nqp::bindattr($hash,Map,'$!storage',$storage)
            )
          ),
          $hash
        )
    }
    multi method hash(Setty:D: --> Hash:D) { self.HASHIFY(Any) }
    multi method Hash(Setty:D: --> Hash:D) { self.HASHIFY(Bool) }

    multi method ACCEPTS(Setty:U: $other) {
        $other.^does(self)
    }
    multi method ACCEPTS(Setty:D: Seq:D \seq) {
        self.ACCEPTS(seq.list)
    }
    multi method ACCEPTS(Setty:D: $other) {
        $other (<=) self && self (<=) $other
    }

    multi method Str(Setty:D $ : --> Str:D) { ~ self.hll_hash.values }
    multi method gist(Setty:D $ : --> Str:D) {
        my $name := self.^name;
        ( $name eq 'Set' ?? 'set' !! "$name.new" )
        ~ '('
        ~ self.hll_hash.values.map( {.gist} ).join(', ')
        ~ ')';
    }
    multi method perl(Setty:D $ : --> Str:D) {
        my $name := self.^name;
        ( $name eq 'Set' ?? 'set' !! "$name.new" )
        ~ '('
        ~ self.hll_hash.values.map( {.perl} ).join(',')
        ~ ')';
    }

    proto method grab(|) { * }
    proto method grabpairs(|) { * }

    proto method pick(|) { * }
    multi method pick(Setty:D:) { self.roll }
    multi method pick(Setty:D: Callable:D $calculate) {
        self.pick( $calculate(self.elems) )
    }
    multi method pick(Setty:D: Whatever $) {
        self.pick(Inf)
    }
    multi method pick(Setty:D: $count) {
        Seq.new(class :: does Rakudo::QuantHash::Pairs {
            method pull-one() is raw {
                nqp::if(
                  nqp::elems($!picked),
                  nqp::atkey($!elems,nqp::pop_s($!picked)),
                  IterationEnd
                )
            }
        }.new($!elems, $count))
    }

    proto method pickpairs(|) { * }
    multi method pickpairs(Setty:D:) { Pair.new(self.roll,True) }
    multi method pickpairs(Setty:D: Callable:D $calculate) {
        self.pickpairs( $calculate(self.elems) )
    }
    multi method pickpairs(Setty:D: Whatever $) {
        self.pickpairs(Inf)
    }
    multi method pickpairs(Setty:D: $count) {
        Seq.new(class :: does Rakudo::QuantHash::Pairs {
            method pull-one() is raw {
                nqp::if(
                  nqp::elems($!picked),
                  Pair.new(nqp::atkey($!elems,nqp::pop_s($!picked)),True),
                  IterationEnd
                )
            }
        }.new($!elems, $count))
    }

    proto method roll(|) { * }
    multi method roll(Setty:D:) {
        nqp::if(
          $!elems,
          nqp::iterval(Rakudo::QuantHash.ROLL($!elems)),
          Nil
        )
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
            (my $keys := self.raw_keys),
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
        nqp::p6bool($!elems && nqp::existskey($!elems,k.WHICH))
    }

    sub BAGGIFY(\setty, \type) {
        nqp::if(
          (my $raw := setty.raw_hash) && nqp::elems($raw),
          nqp::stmts(
            (my $elems := nqp::clone($raw)),
            (my $iter := nqp::iterator($elems)),
            nqp::while(
              $iter,
              nqp::bindkey(
                $elems,
                nqp::iterkey_s(nqp::shift($iter)),
                Pair.new(nqp::decont(nqp::iterval($iter)),1)
              )
            ),
            nqp::create(type).SET-SELF($elems)
          ),
          nqp::create(type)
        )
    }
    multi method Bag(Setty:D:)     { BAGGIFY(self, Bag)     }
    multi method BagHash(Setty:D:) { BAGGIFY(self, BagHash) }
    multi method Mix(Setty:D:)     { BAGGIFY(self, Mix)     }
    multi method MixHash(Setty:D:) { BAGGIFY(self, MixHash) }

    method raw_hash() is raw { $!elems }
    method hll_hash() is raw {
        nqp::p6bindattrinvres(nqp::create(Hash),Map,'$!storage',$!elems)
    }

    # TODO: WHICH will require the capability for >1 pointer in ObjAt
}

# vim: ft=perl6 expandtab sw=4
