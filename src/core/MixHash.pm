my class MixHash does Mixy {

#--- interface methods
    multi method WHICH(MixHash:D:) { self.Mu::WHICH }
    multi method AT-KEY(MixHash:D: \k) is raw {
        Proxy.new(
          FETCH => {
              nqp::if(
                (my $elems := nqp::getattr(%!elems,Map,'$!storage')),
                nqp::if(
                  nqp::existskey($elems,(my $which := k.WHICH)),
                  nqp::getattr(
                    nqp::decont(nqp::atkey($elems,$which)),
                    Pair,
                    '$!value'
                  ),
                  0
                ),
                0
              )
          },
          STORE => -> $, Real() $value {
              nqp::if(
                nqp::istype($value,Failure),   # RT 128927
                $value.throw,
                nqp::if(
                  (my $elems := nqp::getattr(%!elems,Map,'$!storage')),
                  nqp::if(                      # allocated hash
                    nqp::existskey($elems,(my $which := k.WHICH)),
                    nqp::if(                    # existing element
                      $value == 0,
                      nqp::stmts(
                        nqp::deletekey($elems,$which),
                        0
                      ),
                      (nqp::getattr(
                        nqp::decont(nqp::atkey($elems,$which)),
                        Pair,
                        '$!value'
                      ) = $value),
                    ),
                    nqp::unless(
                      $value == 0,
                      nqp::bindkey($elems,$which,self!PAIR(k,$value))  # new
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
    method Mix(:$view) {
        nqp::if(
          nqp::getattr(%!elems,Map,'$!storage'),
          nqp::p6bindattrinvres(
            nqp::create(Mix),Mix,'%!elems',
            nqp::if($view,%!elems,%!elems.clone)
          ),
          nqp::create(Mix)
        )
    }
    method MixHash { self }
    method clone(MixHash:D:) { self.new-from-pairs(self.pairs) }
}

# vim: ft=perl6 expandtab sw=4
