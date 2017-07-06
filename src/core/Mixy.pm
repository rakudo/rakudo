my role Mixy does Baggy  {

    multi method hash(Mixy:D: --> Hash:D) { self.HASHIFY(Any) }
    multi method Hash(Mixy:D: --> Hash:D) { self.HASHIFY(Real) }

    multi method kxxv(Mixy:D:) {
        Failure.new(".kxxv is not supported on a {self.^name}")
    }

    multi method grab(Mixy:D: $count?) {
        Failure.new(".grab is not supported on a {self.^name}")
    }

    multi method pick(Mixy:D: $count?) {
        Failure.new(".pick is not supported on a {self.^name}")
    }

    multi method roll(Mixy:D:) {
        nqp::if(
          (my $total :=
            Rakudo::QuantHash.MIX-TOTAL-POSITIVE(my $raw := self.RAW-HASH)),
          nqp::getattr(
            nqp::iterval(Rakudo::QuantHash.MIX-ROLL($raw,$total)),
            Pair,
            '$!key'
          ),
          Nil
        )
    }
    multi method roll(Mixy:D: Whatever) { self.roll(Inf) }
    multi method roll(Mixy:D: Callable:D $calculate) {
      nqp::if(
        (my $total := Rakudo::QuantHash.MIX-TOTAL-POSITIVE(self.RAW-HASH)),
        self.roll($calculate($total)),
        Seq.new(Rakudo::Iterator.Empty)
      )
    }
    multi method roll(Mixy:D: $count) {
        Seq.new(nqp::if(
          (my $total :=
            Rakudo::QuantHash.MIX-TOTAL-POSITIVE(my $raw := self.RAW-HASH)),
          nqp::stmts(
            (my $done = 0),
            Rakudo::Iterator.Callable( {
                nqp::if(
                  $done++ < $count,
                  nqp::getattr(
                    nqp::iterval(Rakudo::QuantHash.MIX-ROLL($raw,$total)),
                    Pair,
                    '$!key'
                  ),
                  IterationEnd
                )
            })
          ),
          Rakudo::Iterator.Empty
        ))
    }

#--- object creation methods
    method new-from-pairs(Mixy:_: *@pairs --> Mixy:D) {
        nqp::if(
          (my $iterator := @pairs.iterator).is-lazy,
          Failure.new(X::Cannot::Lazy.new(:action<coerce>,:what(self.^name))),
          nqp::create(self).SET-SELF(
            Rakudo::QuantHash.ADD-PAIRS-TO-MIX(
              nqp::create(Rakudo::Internals::IterationSet),$iterator
            )
          )
        )
    }

#--- coercion methods
   sub SETIFY(\mixy, \type) {
        nqp::if(
          (my $raw := mixy.RAW-HASH) && nqp::elems($raw),
          nqp::stmts(
            (my $elems := nqp::clone($raw)),
            (my $iter := nqp::iterator($elems)),
            nqp::while(
              $iter,
              nqp::if(
                nqp::getattr(
                  nqp::iterval(nqp::shift($iter)),
                  Pair,
                  '$!value'
                ) < 0,
                nqp::deletekey($elems,nqp::iterkey_s($iter)),
                nqp::bindkey(
                  $elems,
                  nqp::iterkey_s($iter),
                  nqp::getattr(nqp::iterval($iter),Pair,'$!key')
                )
              )
            ),
            nqp::create(type).SET-SELF($elems)
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
          (my $raw := mixy.RAW-HASH) && nqp::elems($raw),
          nqp::stmts(                               # something to coerce
            (my $elems := nqp::clone($raw)),
            (my $iter := nqp::iterator($elems)),
            nqp::while(
              $iter,
              nqp::if(
                (my $value := nqp::getattr(
                  nqp::iterval(nqp::shift($iter)),
                  Pair,
                  '$!value'
                ).Int) > 0,                         # .Int also deconts
                nqp::bindkey(                       # ok to keep value.Int
                  $elems,
                  nqp::iterkey_s($iter),
                  nqp::p6bindattrinvres(
                    nqp::iterval($iter),Pair,'$!value',$value)
                ),
                nqp::deletekey($elems,nqp::iterkey_s($iter))
              )
            ),
            nqp::create(type).SET-SELF($elems),
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

# vim: ft=perl6 expandtab sw=4
