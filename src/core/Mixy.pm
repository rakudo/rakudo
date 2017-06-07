my role Mixy does Baggy  {

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
    method new-from-pairs(*@pairs) {
        nqp::stmts(
          (my $elems := nqp::create(Rakudo::Internals::IterationSet)),
          (my $iterator := @pairs.iterator),
          nqp::until(
            nqp::eqaddr(
              (my $pulled := nqp::decont($iterator.pull-one)),
              IterationEnd
            ),
            nqp::if(
              nqp::istype($pulled,Pair),
              nqp::stmts(
                (my int $seen-pair = 1),
                nqp::if(
                  nqp::existskey(
                    $elems,
                    (my $which := nqp::getattr($pulled,Pair,'$!key').WHICH)
                  ),
                  nqp::stmts(
                    (my $pair := nqp::atkey($elems,$which)),
                    nqp::bindattr(
                      $pair,
                      Pair,
                      '$!value',
                      nqp::getattr($pair,Pair,'$!value')
                        + nqp::getattr($pulled,Pair,'$!value')
                    )
                  ),
                  nqp::bindkey(
                    $elems,
                    $which,
                    nqp::p6bindattrinvres(
                      nqp::clone($pulled),
                      Pair,
                      '$!value',
                      nqp::decont(nqp::getattr($pulled,Pair,'$!value'))
                    )
                  )
                )
              ),
              nqp::if(
                nqp::existskey(
                  $elems,
                  ($which := $pulled.WHICH)
                ),
                nqp::stmts(
                  ($pair := nqp::atkey($elems,$which)),
                  nqp::bindattr(
                    $pair,
                    Pair,
                    '$!value',
                    nqp::getattr($pair,Pair,'$!value') + 1
                  )
                ),
                nqp::bindkey($elems,$which,Pair.new($pulled,1))
              )
            )
          ),
          nqp::if($seen-pair && nqp::elems($elems),self.SANITY($elems)),
          nqp::create(self).SET-SELF($elems)
        )
    }
}

# vim: ft=perl6 expandtab sw=4
