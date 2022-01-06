my class BagHash does Baggy {

    method ^parameterize(Mu \base, Mu \type) {
        Rakudo::Internals.PARAMETERIZE-KEYOF(base,type)
    }

#--- interface methods
    multi method STORE(BagHash:D: Iterable:D \iterable --> BagHash:D) {
        (my \iterator := iterable.iterator).is-lazy
          ?? self.fail-iterator-cannot-be-lazy('initialize')
          !! self.SET-SELF(
               Rakudo::QuantHash.ADD-PAIRS-TO-BAG(
                 nqp::create(Rakudo::Internals::IterationSet),
                 iterator,
                 self.keyof
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
                # 0 because the value of the condition is returned
              )
          },
          STORE => -> $, Int() $value {
              nqp::if(
                nqp::istype($value,Failure),    # https://github.com/Raku/old-issue-tracker/issues/5567
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
    multi method Bag(BagHash:D: :$view) {  # :view is implementation-detail
        $!elems && nqp::elems($!elems)
          ?? nqp::create(Bag).SET-SELF(                     # not empty
               $view
                 ?? $!elems                                 # won't change
                 !! Rakudo::QuantHash.BAGGY-CLONE($!elems)  # need deep copy
             )
          !! bag()                                          # empty
    }
    multi method BagHash(BagHash:D:) { self }
    multi method Mix(BagHash:D:) {
        $!elems && nqp::elems($!elems)
          ?? nqp::create(Mix).SET-SELF(Rakudo::QuantHash.BAGGY-CLONE($!elems))
          !! mix()
    }
    multi method MixHash(BagHash:D:) {
        $!elems && nqp::elems($!elems)
          ?? nqp::create(MixHash).SET-SELF(
               Rakudo::QuantHash.BAGGY-CLONE($!elems))
          !! nqp::create(MixHash)
    }

    multi method Setty(BagHash:U:) { SetHash      }
    multi method Setty(BagHash:D:) { self.SetHash }
    multi method Baggy(BagHash:U:) { BagHash      }
    multi method Baggy(BagHash:D:) { self         }
    multi method Mixy (BagHash:U:) { MixHash      }
    multi method Mixy (BagHash:D:) { self.MixHash }

    method clone() {
        $!elems && nqp::elems($!elems)
          ?? nqp::create(self).SET-SELF(Rakudo::QuantHash.BAGGY-CLONE($!elems))
          !! nqp::create(self)
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

        # save object for potential recreation
        my $pair := nqp::atkey(elems,$key);

        Proxy.new(
          FETCH => {
              nqp::if(
                nqp::existskey(elems,$key),
                nqp::getattr(nqp::atkey(elems,$key),Pair,'$!value'),
                # 0 the value of existskey if the key doesn't exist
              )
          },
          STORE => -> $, Int() $value {
              nqp::if(
                # https://github.com/Raku/old-issue-tracker/issues/5567
                nqp::istype($value,Failure),
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
    }

    my class Iterate does Iterator {
        has $!elems is built(:bind);
        has $!keys  is built(:bind) is built(False) =
          Rakudo::Internals.IterationSet2keys($!elems);
        method pull-one() is raw {
            nqp::elems($!keys)
              ?? nqp::p6bindattrinvres(
                   nqp::clone(
                     nqp::atkey($!elems,(my $key := nqp::shift_s($!keys)))
                   ),
                   Pair,
                   '$!value',
                   proxy($key,$!elems)
                 )
              !! IterationEnd
        }
        method push-all(\target --> IterationEnd) {
            my $elems := $!elems;
            my $keys  := $!keys;
            nqp::while(  # doesn't sink
              nqp::elems($keys),
              target.push(nqp::atkey($elems,nqp::shift_s($keys)))
            )
        }
    }
    multi method iterator(BagHash:D:) { Iterate.new(:$!elems) }  # also .pairs

    my class KV does Iterator {
        has $!elems is built(:bind);
        has $!keys  is built(:bind) is built(False) =
          Rakudo::Internals.IterationSet2keys($!elems);
        has str $!on;
        method pull-one() is raw {
            nqp::if(
              $!on,
              nqp::stmts(
                (my $proxy := proxy($!on,$!elems)),
                ($!on = ""),
                $proxy
              ),
              nqp::if(
                nqp::elems($!keys),
                nqp::getattr(
                  nqp::atkey($!elems,($!on = nqp::shift_s($!keys))),Pair,'$!key'
                ),
                IterationEnd
              )
            )
        }
        method push-all(\target --> IterationEnd) {
            my $elems := $!elems;
            my $keys  := $!keys;
            nqp::while(
              nqp::elems($keys),
              nqp::stmts(  # doesn't sink
                (my $pair := nqp::atkey($elems,nqp::shift_s($keys))),
                target.push(nqp::getattr($pair,Pair,'$!key')),
                target.push(nqp::getattr($pair,Pair,'$!value'))
              )
            )
        }
    }
    multi method kv(BagHash:D:) { Seq.new(KV.new(:$!elems)) }

    my class Values does Iterator {
        has $!elems is built(:bind);
        has $!keys  is built(:bind) is built(False) =
          Rakudo::Internals.IterationSet2keys($!elems);
        method pull-one() is raw {
            nqp::elems($!keys)
              ?? proxy(nqp::shift_s($!keys),$!elems)
              !! IterationEnd
        }
        method push-all(\target --> IterationEnd) {
            my $elems := $!elems;
            my $keys  := $!keys;
            nqp::while(  # doesn't sink
              nqp::elems($keys),
              target.push(proxy(nqp::shift_s($keys),$elems))
            );
        }
    }
    multi method values(BagHash:D:) { Seq.new(Values.new(:$!elems)) }

#---- selection methods
    multi method grab(BagHash:D:) {
        $!elems && nqp::elems($!elems)
          ?? Rakudo::QuantHash.BAG-GRAB($!elems,self.total)
          !! Nil
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

#--- convenience methods
    method add(BagHash:D: \to-add --> Nil) {
        nqp::bindattr(
          self,SetHash,'$!elems',nqp::create(Rakudo::Internals::IterationSet)
        ) unless $!elems;
        Rakudo::QuantHash.ADD-ITERATOR-TO-BAG(
          $!elems, to-add.iterator, self.keyof
        );
    }

    method remove(BagHash:D: \to-remove --> Nil) {
        Rakudo::QuantHash.SUB-ITERATOR-FROM-BAG(
          $!elems, to-remove.iterator
        ) if $!elems;
    }
}

# vim: expandtab shiftwidth=4
