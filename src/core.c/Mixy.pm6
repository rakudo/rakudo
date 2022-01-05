my role Mixy does Baggy  {

    method of() { Real }

    multi method hash(Mixy:D: --> Hash:D) { self!HASHIFY(Real) }
    multi method Hash(Mixy:D: --> Hash:D) { self!HASHIFY(Any) }

    multi method kxxv(Mixy:D:) {
        Failure.new(".kxxv is not supported on a {self.^name}")
    }

    multi method grab(Mixy:D: $count?) {
        Failure.new(".grab is not supported on a {self.^name}")
    }

    multi method pick(Mixy:D: $count?) {
        Failure.new(".pick is not supported on a {self.^name}, maybe use .roll instead?")
    }

    multi method roll(Mixy:D:) {
        (my \raw := self.RAW-HASH) && (my \total := self!total-positive)
          ?? nqp::getattr(
               nqp::iterval(Rakudo::QuantHash.MIX-ROLL(raw,total)),Pair,'$!key'
             )
          !! Nil
    }
    multi method roll(Mixy:D: Whatever) {
        Seq.new(
          (my \raw := self.RAW-HASH) && (my \total := self!total-positive)
            ?? Rakudo::Iterator.Callable( {
                   nqp::getattr(
                    nqp::iterval(Rakudo::QuantHash.MIX-ROLL(raw,total)),
                    Pair,
                    '$!key'
                   )
               }, True )
            !! Rakudo::Iterator.Empty
        )
    }
    multi method roll(Mixy:D: Callable:D $calculate) {
      (my $total := self!total-positive)
        ?? self.roll($calculate($total))
        !! Seq.new(Rakudo::Iterator.Empty)
    }
    multi method roll(Mixy:D: $count) {
        $count == Inf
          ?? self.roll(*)                        # let Whatever handle it
          !! Seq.new(                            # something else as count
               (my $todo = $count.Int) < 1       # also handles NaN
               ?? Rakudo::Iterator.Empty         # nothing to do
               !! (my \raw := self.RAW-HASH)
                    && (my \total := self!total-positive)
                    && ++$todo
                 ?? Rakudo::Iterator.Callable( { # need to do a number of times
                        --$todo
                          ?? nqp::getattr(
                               nqp::iterval(
                                 Rakudo::QuantHash.MIX-ROLL(raw,total)
                               ),
                               Pair,
                              '$!key'
                             )
                          !! IterationEnd
                    } )
                 !! Rakudo::Iterator.Empty       # nothing to roll for
             )
    }

#--- object creation methods
    method new-from-pairs(Mixy:_: *@pairs --> Mixy:D) {
        (my \iterator := @pairs.iterator).is-lazy
          ?? self.fail-iterator-cannot-be-lazy('coerce')
          !! nqp::create(self).SET-SELF(
               Rakudo::QuantHash.ADD-PAIRS-TO-MIX(
                 nqp::create(Rakudo::Internals::IterationSet),
                 iterator,
                 self.keyof
               )
             )
    }

#--- coercion methods
   sub SETIFY(\mixy, \type) {
        nqp::if(
          (my \raw := mixy.RAW-HASH) && nqp::elems(raw),
          nqp::stmts(
            (my \elems := nqp::clone(raw)),
            (my \iter := nqp::iterator(elems)),
            nqp::while(
              iter,
              nqp::if(
                nqp::getattr(nqp::iterval(nqp::shift(iter)),Pair,'$!value') < 0,
                nqp::deletekey(elems,nqp::iterkey_s(iter)),
                nqp::bindkey(
                  elems,
                  nqp::iterkey_s(iter),
                  nqp::getattr(nqp::iterval(iter),Pair,'$!key')
                )
              )
            ),
            nqp::create(type).SET-SELF(elems)
          ),
          nqp::if(
            nqp::eqaddr(type,Set),
            set(),
            nqp::create(type)
          )
        )
    }
    multi method Set(Mixy:D:)     { SETIFY(self,Set)     }
    multi method SetHash(Mixy:D:) { SETIFY(self,SetHash) }

    sub BAGGIFY(\mixy, \type) {
        nqp::if(
          (my \raw := mixy.RAW-HASH) && nqp::elems(raw),
          nqp::stmts(                               # something to coerce
            (my \elems := nqp::clone(raw)),
            (my \iter := nqp::iterator(elems)),
            nqp::while(
              iter,
              nqp::if(
                (my \value := nqp::getattr(
                  nqp::iterval(nqp::shift(iter)),Pair,'$!value'
                ).Int) > 0,                         # .Int also deconts
                nqp::bindkey(                       # ok to keep value.Int
                  elems,
                  nqp::iterkey_s(iter),
                  nqp::p6bindattrinvres(
                    nqp::iterval(iter),Pair,'$!value',value)
                ),
                nqp::deletekey(elems,nqp::iterkey_s(iter))
              )
            ),
            nqp::create(type).SET-SELF(elems),
          ),
          nqp::if(                                  # nothing to coerce
            nqp::istype(type,Bag),
            bag(),
            nqp::create(BagHash)
          )
        )
    }

    multi method Bag(Baggy:D:)     { BAGGIFY(self, Bag)     }
    multi method BagHash(Baggy:D:) { BAGGIFY(self, BagHash) }
}

# vim: expandtab shiftwidth=4
