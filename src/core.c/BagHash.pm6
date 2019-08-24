my class BagHash does Baggy {

    method ^parameterize(Mu \base, Mu \type) {
        Rakudo::Internals.PARAMETERIZE-KEYOF(base,type)
    }

#--- interface methods
    multi method STORE(BagHash:D: *@pairs --> BagHash:D) {
        nqp::if(
          (my \iterator := @pairs.iterator).is-lazy,
          Failure.new(
            X::Cannot::Lazy.new(:action<initialize>,:what(self.^name))
          ),
          self.SET-SELF(
            Rakudo::QuantHash.ADD-PAIRS-TO-BAG(
              nqp::create(Rakudo::Internals::IterationSet),iterator,self.keyof
            )
          )
        )
    }
    multi method STORE(BagHash:D: \objects, \values --> BagHash:D) {
        self.SET-SELF(
          Rakudo::QuantHash.ADD-OBJECTS-VALUES-TO-BAG(
            nqp::create(Rakudo::Internals::IterationSet),
            objects.iterator,
            values.iterator,
            self.keyof
          )
        )
    }
    multi method AT-KEY(BagHash:D: \k) is raw {
        my \type := self.keyof;
        Proxy.new(
          FETCH => {
              nqp::if(
                nqp::istrue($!elems)
                  && nqp::existskey($!elems,(my $which := k.WHICH)),
                nqp::getattr(nqp::atkey($!elems,$which),Pair,'$!value'),
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
                      $value > 0,
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
                      $value > 0,               # new
                      Rakudo::QuantHash.BIND-TO-TYPED-BAG(
                        $!elems, $which, k, nqp::decont($value), type
                      )
                    )
                  ),
                  nqp::if(                      # no hash allocated yet
                    $value > 0,
                    Rakudo::QuantHash.BIND-TO-TYPED-BAG(
                      nqp::bindattr(self,BagHash,'$!elems',
                        nqp::create(Rakudo::Internals::IterationSet)
                      ),
                      k.WHICH, k, nqp::decont($value), type
                    )
                  )
                )
              )
          }
        )
    }

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

    multi method Setty(BagHash:U:) { SetHash      }
    multi method Setty(BagHash:D:) { self.SetHash }
    multi method Baggy(BagHash:U:) { BagHash      }
    multi method Baggy(BagHash:D:) { self         }
    multi method Mixy (BagHash:U:) { MixHash      }
    multi method Mixy (BagHash:D:) { self.MixHash }

    method clone() {
        nqp::if(
          $!elems && nqp::elems($!elems),
          nqp::create(self).SET-SELF(Rakudo::QuantHash.BAGGY-CLONE($!elems)),
          nqp::create(self)
        )
    }

#--- iterator methods

    sub proxy(str $key, Mu \elems) is raw {
        # We are only sure that the key exists when the Proxy
        # is made, but we cannot be sure of its existence when
        # either the FETCH or STORE block is executed.  So we
        # still need to check for existence, and handle the case
        # where we need to (re-create) the key and value.  The
        # logic is therefore basically the same as in AT-KEY,
        # except for tests for allocated storage and .WHICH
        # processing.
        nqp::stmts(
          # save object for potential recreation
          (my $pair := nqp::atkey(elems,$key)),

          Proxy.new(
            FETCH => {
                nqp::if(
                  nqp::existskey(elems,$key),
                  nqp::getattr(nqp::atkey(elems,$key),Pair,'$!value'),
                  0
                )
            },
            STORE => -> $, Int() $value {
                nqp::if(
                  nqp::istype($value,Failure),  # RT 128927
                  $value.throw,
                  nqp::if(
                    nqp::existskey(elems,$key),
                    nqp::if(                    # existing element
                      nqp::isgt_i($value,0),
                      nqp::bindattr(            # value ok
                        nqp::atkey(elems,$key),
                        Pair,
                        '$!value',
                        nqp::decont($value)
                      ),
                      nqp::stmts(               # goodbye!
                        nqp::deletekey(elems,$key),
                        0
                      )
                    ),
                    nqp::if(                    # where did it go?
                      nqp::isgt_i($value,0),
                      nqp::bindattr(
                        nqp::bindkey(elems,$key,$pair),
                        Pair,
                        '$!value',
                        nqp::decont($value)
                      )
                    )
                  )
                )
            }
          )
        )
    }

    my class Iterate does Rakudo::Iterator::Mappy {
        method !SET-SELF(\elems) {
            nqp::bind($!hash,elems);
            nqp::bind($!iter,Rakudo::Internals.ITERATIONSET2LISTITER(elems));
            self
        }
        method pull-one() is raw {
            nqp::if(
              $!iter,
              nqp::p6bindattrinvres(
                nqp::clone(nqp::atkey($!hash,(my $key := nqp::shift($!iter)))),
                Pair,
                '$!value',
                proxy($key,$!hash)
              ),
              IterationEnd
            )
        }
        method push-all(\target --> IterationEnd) {
            nqp::while(  # doesn't sink
              $!iter,
              target.push(nqp::atkey($!hash,nqp::shift($!iter)))
            )
        }
    }
    multi method iterator(BagHash:D:) { Iterate.new($!elems) }  # also .pairs

    my class KV does Rakudo::Iterator::Mappy-kv-from-pairs {
        method !SET-SELF(Mu \elems) {
            nqp::bind($!hash,elems);
            nqp::bind($!iter,Rakudo::Internals.ITERATIONSET2LISTITER(elems));
            self
        }
        method pull-one() is raw {
            nqp::if(
              $!on,
              nqp::stmts(
                (my $proxy := proxy($!on,$!hash)),
                ($!on = ""),
                $proxy
              ),
              nqp::if(
                $!iter,
                nqp::getattr(
                  nqp::atkey($!hash,($!on= nqp::shift($!iter))),Pair,'$!key'
                ),
                IterationEnd
              )
            )
        }
        method skip-one() {  # the one provided by the role interferes
            nqp::not_i(nqp::eqaddr(self.pull-one,IterationEnd))
        }
        method push-all(\target --> IterationEnd) {
            nqp::while(
              $!iter,
              nqp::stmts(  # doesn't sink
                (my $pair := nqp::atkey($!hash,nqp::shift($!iter))),
                target.push(nqp::getattr($pair,Pair,'$!key')),
                target.push(nqp::getattr($pair,Pair,'$!value'))
              )
            )
        }
    }
    multi method kv(BagHash:D:) { Seq.new(KV.new($!elems)) }

    my class Values does Rakudo::Iterator::Mappy {
        method !SET-SELF(\elems) {
            nqp::bind($!hash,elems);
            nqp::bind($!iter,Rakudo::Internals.ITERATIONSET2LISTITER(elems));
            self
        }
        method pull-one() is raw {
            nqp::if(
              $!iter,
              proxy(nqp::shift($!iter),$!hash),
              IterationEnd
            )
        }

        method push-all(\target --> IterationEnd) {
            nqp::while(  # doesn't sink
              $!iter,
              target.push(proxy(nqp::shift($!iter),$!hash))
            )
        }
    }
    multi method values(BagHash:D:) { Seq.new(Values.new($!elems)) }

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
