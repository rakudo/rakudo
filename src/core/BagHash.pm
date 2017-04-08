my class BagHash does Baggy {

#--- interface methods
    multi method WHICH(BagHash:D:) { self.Mu::WHICH }
    multi method AT-KEY(BagHash:D: \k) is raw {
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
          STORE => -> $, Int() $value {
              nqp::if(
                nqp::istype($value,Failure),   # RT 128927
                $value.throw,
                nqp::if(
                  (my $elems := nqp::getattr(%!elems,Map,'$!storage')),
                  nqp::if(                      # allocated hash
                    nqp::existskey($elems,(my $which := k.WHICH)),
                    nqp::if(                    # existing element
                      nqp::isgt_i($value,0),
                      (nqp::getattr(
                        nqp::decont(nqp::atkey($elems,$which)),
                        Pair,
                        '$!value'
                      ) = $value),
                      nqp::stmts(
                        nqp::deletekey($elems,$which),
                        0
                      )
                    ),
                    nqp::if(
                      nqp::isgt_i($value,0),
                      nqp::bindkey($elems,$which,self!PAIR(k,$value))  # new
                    )
                  ),
                  nqp::if(                      # no hash allocated yet
                    nqp::isgt_i($value,0),
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

#--- introspection methods
    method Bag(:$view) is nodal {
        nqp::if(
          nqp::getattr(%!elems,Map,'$!storage'),
          nqp::p6bindattrinvres(
            nqp::create(Bag),Bag,'%!elems',
            nqp::if($view,%!elems,%!elems.clone)
          ),
          nqp::create(Bag)
        )
    }
    method BagHash() is nodal { self }
    method Mix {
        nqp::if(
          nqp::getattr(%!elems,Map,'$!storage'),
          nqp::p6bindattrinvres(nqp::create(Mix),Mix,'%!elems',%!elems.clone),
          nqp::create(Mix)
        )
    }
    method MixHash { MixHash.new-from-pairs(%!elems.values) }
    method clone(BagHash:D:) { self.new-from-pairs(self.pairs) }
}

# vim: ft=perl6 expandtab sw=4
