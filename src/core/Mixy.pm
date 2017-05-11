my role Mixy does Baggy  {

    method !PAIR(\key,\value) { Pair.new(key, my Real $ = value ) }
    method SANITY(\elems --> Nil) {
       nqp::stmts(
          (my $iter := nqp::iterator(elems)),
          nqp::while(
            $iter,
            nqp::if(
              nqp::getattr(
                nqp::iterval(nqp::shift($iter)),
                Pair,
                '$!value'
              ) == 0,
              nqp::deletekey(elems,nqp::iterkey_s($iter))
            )
          )
        )
    }

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
            Rakudo::QuantHash.MIX-TOTAL-POSITIVE(my $raw := self.raw_hash)),
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
        (my $total := Rakudo::QuantHash.MIX-TOTAL-POSITIVE(self.raw_hash)),
        self.roll($calculate($total)),
        Seq.new(Rakudo::Iterator.Empty)
      )
    }
    multi method roll(Mixy:D: $count) {
        Seq.new(nqp::if(
          (my $total :=
            Rakudo::QuantHash.MIX-TOTAL-POSITIVE(my $raw := self.raw_hash)),
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
}

# vim: ft=perl6 expandtab sw=4
