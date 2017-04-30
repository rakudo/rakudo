my class MixHash does Mixy {

#--- interface methods
    multi method WHICH(MixHash:D:) { self.Mu::WHICH }
    method total() { Rakudo::QuantHash.MIX-TOTAL(self.raw_hash) }

    multi method AT-KEY(MixHash:D: \k) is raw {
        Proxy.new(
          FETCH => {
              nqp::if(
                (my $raw := self.raw_hash)
                  && nqp::existskey($raw,(my $which := k.WHICH)),
                nqp::getattr(
                  nqp::decont(nqp::atkey($raw,$which)),
                  Pair,
                  '$!value'
                ),
                0
              )
          },
          STORE => -> $, Real() $value {
              nqp::if(
                nqp::istype($value,Failure),   # RT 128927
                $value.throw,
                nqp::if(
                  (my $raw := self.raw_hash),
                  nqp::if(                      # allocated hash
                    nqp::existskey($raw,(my $which := k.WHICH)),
                    nqp::if(                    # existing element
                      $value == 0,
                      nqp::stmts(
                        nqp::deletekey($raw,$which),
                        0
                      ),
                      (nqp::getattr(
                        nqp::decont(nqp::atkey($raw,$which)),
                        Pair,
                        '$!value'
                      ) = $value),
                    ),
                    nqp::unless(
                      $value == 0,
                      nqp::bindkey($raw,$which,self!PAIR(k,$value))  # new
                    )
                  ),
                  nqp::unless(                  # no hash allocated yet
                    $value == 0,
                    nqp::bindkey(
                      nqp::bindattr(%!elems,Map,'$!storage',
                        nqp::create(Rakudo::Internals::IterationSet)),
                      k.WHICH,
                      self!PAIR(k,$value)
                    )
                  )
                )
              )
          }
        )
    }

#--- coercion methods
    method Mix(:$view) is nodal {
        nqp::if(
          (my $raw := self.raw_hash) && nqp::elems($raw),
          nqp::p6bindattrinvres(
            nqp::create(Mix),Mix,'%!elems',
            nqp::if($view,%!elems,%!elems.clone)
          ),
          mix()
        )
    }
    method MixHash() is nodal { self }
}

# vim: ft=perl6 expandtab sw=4
