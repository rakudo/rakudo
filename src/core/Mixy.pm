my role Mixy does Baggy  {

    method !PAIR(\key,\value) { Pair.new(key, my Real $ = value ) }
    method !SANITY(\elems --> Nil) {
       nqp::stmts(
          (my $iter := nqp::iterator(elems)),
          nqp::while(
            $iter,
            nqp::if(
              nqp::getattr(
                nqp::iterval(my $tmp := nqp::shift($iter)),
                Pair,
                '$!value'
              ) == 0,
              nqp::deletekey(elems,nqp::iterkey_s($tmp))
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
        Rakudo::Internals::WeightedRoll.new(self).roll
    }
    multi method roll(Mixy:D: $count) {
        my $roller = Rakudo::Internals::WeightedRoll.new(self);
        map { $roller.roll }, 1 .. $count;
    }
}

# vim: ft=perl6 expandtab sw=4
