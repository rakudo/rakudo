my class BagHash does Baggy {

#--- interface methods
    multi method AT-KEY(BagHash:D: \k) is raw {
        Proxy.new(
          FETCH => {
              nqp::if(
                $!elems && nqp::existskey($!elems,(my $which := k.WHICH)),
                nqp::getattr(nqp::atkey($!elems,$which),Pair,'$!value'),
                0
              )
          },
          STORE => -> $, Int() $value {
              nqp::if(
                nqp::istype($value,Failure),    # RT 128927
                $value.throw,
                nqp::if(
                  $!elems,
                  nqp::if(                      # allocated hash
                    nqp::existskey($!elems,(my $which := k.WHICH)),
                    nqp::if(                    # existing element
                      nqp::isgt_i($value,0),
                      nqp::bindattr(
                        nqp::atkey($!elems,$which),
                        Pair,
                        '$!value',
                        nqp::decont($value)
                      ),
                      nqp::stmts(
                        nqp::deletekey($!elems,$which),
                        0
                      )
                    ),
                    nqp::if(
                      nqp::isgt_i($value,0),    # new
                      nqp::bindkey(
                        $!elems,
                        $which,
                        Pair.new(k,nqp::decont($value))
                      )
                    )
                  ),
                  nqp::if(                      # no hash allocated yet
                    nqp::isgt_i($value,0),
                    nqp::bindkey(
                      nqp::bindattr(self,::?CLASS,'$!elems',
                        nqp::create(Rakudo::Internals::IterationSet)),
                      k.WHICH,
                      Pair.new(k,nqp::decont($value))
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
    method total() { Rakudo::QuantHash.BAG-TOTAL($!elems) }

#--- coercion methods
    multi method Bag(BagHash:D: :$view) {
        nqp::if(
          $!elems && nqp::elems($!elems),
          nqp::create(Bag).SET-SELF(                  # not empty
            nqp::if(
              $view,
              $!elems,                                # BagHash won't change
              Rakudo::QuantHash.BAGGY-CLONE($!elems)  # need deep copy
            )
          ),
          bag()                                       # empty, bag() will do
        )
    }
    multi method BagHash(BagHash:D:) { self }
    multi method Mix(BagHash:D:) {
        nqp::if(
          $!elems && nqp::elems($!elems),
          nqp::create(Mix).SET-SELF(Rakudo::QuantHash.BAGGY-CLONE($!elems)),
          mix()
        )
    }
    multi method MixHash(BagHash:D:) {
        nqp::if(
          $!elems && nqp::elems($!elems),
          nqp::create(MixHash).SET-SELF(Rakudo::QuantHash.BAGGY-CLONE($!elems)),
          nqp::create(MixHash)
        )
    }
    method clone() {
        nqp::if(
          $!elems && nqp::elems($!elems),
          nqp::create(BagHash).SET-SELF(Rakudo::QuantHash.BAGGY-CLONE($!elems)),
          nqp::create(BagHash)
        )
    }

#--- iterator methods

    sub proxy(Mu \iter,Mu \storage) is raw {
        # We are only sure that the key exists when the Proxy
        # is made, but we cannot be sure of its existence when
        # either the FETCH or STORE block is executed.  So we
        # still need to check for existence, and handle the case
        # where we need to (re-create) the key and value.  The
        # logic is therefore basically the same as in AT-KEY,
        # except for tests for allocated storage and .WHICH
        # processing.
        nqp::stmts(
          (my $which  := nqp::iterkey_s(iter)),
          # save object for potential recreation
          (my $object := nqp::getattr(nqp::iterval(iter),Pair,'$!key')),

          Proxy.new(
            FETCH => {
                nqp::if(
                  nqp::existskey(storage,$which),
                  nqp::getattr(nqp::atkey(storage,$which),Pair,'$!value'),
                  0
                )
            },
            STORE => -> $, Int() $value {
                nqp::if(
                  nqp::istype($value,Failure),  # RT 128927
                  $value.throw,
                  nqp::if(
                    nqp::existskey(storage,$which),
                    nqp::if(                    # existing element
                      nqp::isgt_i($value,0),
                      nqp::bindattr(            # value ok
                        nqp::atkey(storage,$which),
                        Pair,
                        '$!value',
                        nqp::decont($value)
                      ),
                      nqp::stmts(               # goodbye!
                        nqp::deletekey(storage,$which),
                        0
                      )
                    ),
                    nqp::if(                    # where did it go?
                      nqp::isgt_i($value,0),
                      nqp::bindkey(
                        storage,
                        $which,
                        Pair.new($object,nqp::decont($value))
                      )
                    )
                  )
                )
            }
          )
        )
    }

    multi method iterator(BagHash:D:) {    # also .pairs
        class :: does Rakudo::Iterator::Mappy {
            method pull-one() is raw {
                nqp::if(
                  $!iter,
                  nqp::p6bindattrinvres(
                    nqp::clone(nqp::iterval(nqp::shift($!iter))),
                    Pair,
                    '$!value',
                    proxy($!iter,$!hash)
                  ),
                  IterationEnd
                )
            }
            method push-all($target --> IterationEnd) {
                nqp::while(  # doesn't sink
                  $!iter,
                  $target.push(nqp::iterval(nqp::shift($!iter)))
                )
            }
        }.new($!elems)
    }

    multi method values(BagHash:D:) {
        Seq.new(class :: does Rakudo::Iterator::Mappy {
            method pull-one() is raw {
                nqp::if(
                  $!iter,
                  proxy(nqp::shift($!iter),$!hash),
                  IterationEnd
                )
            }

            # same as Baggy.values
            method push-all($target --> IterationEnd) {
                nqp::while(  # doesn't sink
                  $!iter,
                  $target.push(nqp::getattr(
                    nqp::iterval(nqp::shift($!iter)),Pair,'$!value'))
                )
            }
        }.new($!elems))
    }

    multi method kv(BagHash:D:) {
        Seq.new(class :: does Rakudo::Iterator::Mappy-kv-from-pairs {
            method pull-one() is raw {
                nqp::if(
                  $!on,
                  nqp::stmts(
                    ($!on = 0),
                    proxy($!iter,$!hash)
                  ),
                  nqp::if(
                    $!iter,
                    nqp::stmts(
                      ($!on = 1),
                      nqp::getattr(
                        nqp::iterval(nqp::shift($!iter)),Pair,'$!key')
                    ),
                    IterationEnd
                  )
                )
            }
        }.new($!elems))
    }

#---- selection methods
    multi method grab(BagHash:D:) {
        nqp::if(
          $!elems && nqp::elems($!elems),
          Rakudo::QuantHash.BAG-GRAB($!elems,self.total),
          Nil
        )
    }
    multi method grab(BagHash:D: Callable:D $calculate) {
        self.grab( $calculate(self.total) )
    }
    multi method grab(BagHash:D: Whatever) { self.grab(Inf) }
    multi method grab(BagHash:D: $count) {
        Seq.new(nqp::if(
          (my $todo = Rakudo::QuantHash.TODO($count))
            && $!elems
            && nqp::elems($!elems),
          nqp::stmts(
            (my Int $total = self.total),
            nqp::if($todo > $total,$todo = $total),
            Rakudo::Iterator.Callable( {
                nqp::if(
                  $todo,
                  nqp::stmts(
                    --$todo,
                    Rakudo::QuantHash.BAG-GRAB($!elems,$total--)
                  ),
                  IterationEnd
                )
            } )
          ),
          Rakudo::Iterator.Empty
        ))
    }
}

# vim: ft=perl6 expandtab sw=4
