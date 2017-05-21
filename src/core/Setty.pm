my role Setty does QuantHash {
    has $!elems; # key.WHICH => key

    multi method new(Setty: --> Setty:D) { nqp::create(self) }
    multi method new(Setty: +@args --> Setty:D) {
        nqp::if(
          (my $iterator := @args.iterator).is-lazy,
          Failure.new(X::Cannot::Lazy.new(:action<coerce>,:what<Set>)),
          nqp::stmts(
            (my $elems := nqp::create(Rakudo::Internals::IterationSet)),
            (my $iter  := @args.iterator),
            nqp::until(
              nqp::eqaddr((my $pulled := $iter.pull-one),IterationEnd),
              nqp::bindkey($elems,$pulled.WHICH,$pulled)
            ),
            nqp::create(self).SET-SELF($elems)
          )
        )
    }
    method new-from-pairs(*@pairs --> Setty:D) {
        nqp::if(
          (my $iterator := @pairs.iterator).is-lazy,
          Failure.new(X::Cannot::Lazy.new(:action<coerce>,:what<Set>)),
          nqp::create(self).SET-SELF(
            self.fill_IterationSet(
              nqp::create(Rakudo::Internals::IterationSet),$iterator
            )
          )
        )
    }

    method fill_IterationSet(\elems,\iterator) {
        nqp::stmts(
          nqp::until(
            nqp::eqaddr(
              (my $pulled := iterator.pull-one),
              IterationEnd
            ),
            nqp::if(
              nqp::istype($pulled,Pair),
              nqp::if(
                nqp::getattr(nqp::decont($pulled),Pair,'$!value'),
                nqp::bindkey(
                  elems,
                  nqp::getattr(nqp::decont($pulled),Pair,'$!key').WHICH,
                  nqp::getattr(nqp::decont($pulled),Pair,'$!key')
                )
              ),
              nqp::bindkey(elems,$pulled.WHICH,$pulled)
            )
          ),
          elems
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

    multi method hash(Setty:D: --> Hash:D) {
        nqp::stmts(
          (my $hash := Hash.^parameterize(Bool,Any).new),
          (my $descriptor := nqp::getattr($hash,Hash,'$!descriptor')),
          nqp::if(
            $!elems,
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
        self.hll_hash.values.pick($calculate(self.elems))
    }
    multi method pick(Setty:D: $count) { self.hll_hash.values.pick($count) }

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

    method !BAGGIFY(\type) {
        nqp::if(
          $!elems,
          nqp::stmts(
            (my $elems := nqp::clone($!elems)),
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
    multi method Bag(Setty:D:)     { self!BAGGIFY(Bag)     }
    multi method BagHash(Setty:D:) { self!BAGGIFY(BagHash) }
    multi method Mix(Setty:D:)     { self!BAGGIFY(Mix)     }
    multi method MixHash(Setty:D:) { self!BAGGIFY(MixHash) }

    method raw_hash() is raw { $!elems }
    method hll_hash() is raw {
        nqp::p6bindattrinvres(nqp::create(Hash),Map,'$!storage',$!elems)
    }

    # TODO: WHICH will require the capability for >1 pointer in ObjAt
}

# vim: ft=perl6 expandtab sw=4
