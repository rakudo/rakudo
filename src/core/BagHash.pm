my class BagHash does Baggy {

#--- interface methods
    multi method WHICH(BagHash:D:) { self.Mu::WHICH }
    multi method AT-KEY(BagHash:D: \k) is raw {
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
          STORE => -> $, Int() $value {
              nqp::if(
                nqp::istype($value,Failure),   # RT 128927
                $value.throw,
                nqp::if(
                  (my $raw := self.raw_hash),
                  nqp::if(                      # allocated hash
                    nqp::existskey($raw,(my $which := k.WHICH)),
                    nqp::if(                    # existing element
                      nqp::isgt_i($value,0),
                      (nqp::getattr(
                        nqp::decont(nqp::atkey($raw,$which)),
                        Pair,
                        '$!value'
                      ) = $value),
                      nqp::stmts(
                        nqp::deletekey($raw,$which),
                        0
                      )
                    ),
                    nqp::if(
                      nqp::isgt_i($value,0),
                      nqp::bindkey($raw,$which,self!PAIR(k,$value))  # new
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

#--- object creation methods
    multi method new(BagHash:_:) { nqp::create(self) }

#--- introspection methods
    method total() { Rakudo::QuantHash.BAG-TOTAL(self.raw_hash) }

    method Bag(:$view) is nodal {
        nqp::if(
          (my $raw := self.raw_hash) && nqp::elems($raw),
          nqp::p6bindattrinvres(
            nqp::create(Bag),Bag,'%!elems',
            nqp::if($view,%!elems,%!elems.clone)
          ),
          bag()
        )
    }
    method BagHash() is nodal { self }
    method Mix() is nodal {
        nqp::if(
          (my $raw := self.raw_hash) && nqp::elems($raw),
          nqp::p6bindattrinvres(nqp::create(Mix),Mix,'%!elems',%!elems.clone),
          mix()
        )
    }

#--- iterator methods
    multi method values(BagHash:D:) {
        Seq.new(class :: does Rakudo::Iterator::Mappy {
            method pull-one() is raw {
                nqp::if(
                  $!iter,

                  # We are only sure that the key exists when the Proxy
                  # is made, but we cannot be sure of its existence when
                  # either the FETCH or STORE block is executed.  So we
                  # still need to check for existence, and handle the case
                  # where we need to (re-create) the key and value.  The
                  # logic is therefore basically the same as in AT-KEY,
                  # except for tests for allocated storage and .WHICH
                  # processing.
                  nqp::stmts(
                    (my $which := nqp::iterkey_s(nqp::shift($!iter))),
                    (my $pair  := nqp::iterval($!iter)),  # recreation

                    Proxy.new(
                      FETCH => {
                          nqp::if(
                            nqp::existskey($!storage,$which),
                            nqp::getattr(
                              nqp::decont(nqp::atkey($!storage,$which)),
                              Pair,
                              '$!value'
                            ),
                            0
                          )
                      },
                      STORE => -> $, Int() $value {
                          nqp::if(
                            nqp::istype($value,Failure),  # RT 128927
                            $value.throw,
                            nqp::if(
                              nqp::existskey($!storage,$which),
                              nqp::if(                    # existing element
                                nqp::isgt_i($value,0),
                                (nqp::getattr(            # value ok
                                  nqp::decont(nqp::atkey($!storage,$which)),
                                  Pair,
                                  '$!value'
                                ) = $value),
                                nqp::stmts(               # goodbye!
                                  nqp::deletekey($!storage,$which),
                                  0
                                )
                              ),
                              nqp::if(                    # where did it go?
                                nqp::isgt_i($value,0),
                                nqp::stmts(               # ok to add (again)
                                  (nqp::getattr($pair,Pair,'$!value') = $value),
                                  nqp::bindkey($!storage,$which,$pair)
                                )
                              )
                            )
                          )
                      }
                    )
                  ),
                  IterationEnd
                )
            }

            # same as Baggy.values
            method push-all($target --> IterationEnd) {
                nqp::while(  # doesn't sink
                  $!iter,
                  $target.push(nqp::getattr(nqp::decont(
                    nqp::iterval(nqp::shift($!iter))),Pair,'$!value'))
                )
            }
        }.new(%!elems))
   }
}

# vim: ft=perl6 expandtab sw=4
