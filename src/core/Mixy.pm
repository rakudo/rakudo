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
          (my $raw := self.raw_hash) && nqp::elems($raw),
          nqp::stmts(
            (my $total := 0),
            (my $iter := nqp::iterator($raw)),
            nqp::while(
              $iter,
              nqp::if(
                0 < (my $value :=
                  nqp::getattr(nqp::iterval(nqp::shift($iter)),Pair,'$!value')),
                ($total := $total + $value)
              )
            ),
            nqp::if(
              $total,
              nqp::getattr(
                nqp::iterval(Rakudo::QuantHash.MIX-ROLL($raw,$total)),
                Pair,
                '$!key'
              ),
              Nil
            )
          ),
          Nil
        )
    }
    multi method roll(Mixy:D: $count) {
        my $roller = Rakudo::QuantHash::WeightedRoll.new(self);
        map { $roller.roll }, 1 .. $count;
    }
}

# vim: ft=perl6 expandtab sw=4
