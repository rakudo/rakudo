my class BagHash does Baggy {

    method ^parameterize(Mu \base, Mu \type) {
        my \what := base.^mixin(QuantHash::KeyOf[type]);
        what.^set_name(
          nqp::concat(base.^name,'[') ~ nqp::concat(type.^name,']')
        );
        what
    }

#--- interface methods
    multi method STORE(BagHash:D: Any:D \keys --> BagHash:D) {
        (my \iterator := keys.iterator).is-lazy
          ?? self.fail-iterator-cannot-be-lazy('initialize')
          !! self.SETUP(
               Rakudo::QuantHash.ADD-PAIRS-TO-BAG(
                 nqp::create(Rakudo::Internals::IterationSet),
                 iterator,
                 self.OBJECTIFIER
               )
             )
    }
    multi method STORE(BagHash:D: \objects, \values --> BagHash:D) {
        self.SETUP(
          Rakudo::QuantHash.ADD-OBJECTS-VALUES-TO-BAG(
            nqp::create(Rakudo::Internals::IterationSet),
            objects.iterator,
            values.iterator,
            self.OBJECTIFIER
          )
        )
    }

    multi method AT-KEY(BagHash:D: \k) is raw {
        my     $object := self.OBJECTIFIER()(k);
        my str $which   = $object.WHICH;

        Proxy.new(
          FETCH => {
              (my $pair := nqp::atkey($!elems,$which))
                ?? $pair.value
                !! 0
          },
          STORE => -> $, Int() $value {
              my $elems  := $!elems;

              nqp::if(
                # https://github.com/Raku/old-issue-tracker/issues/5567
                nqp::istype($value,Failure),
                $value.throw,
                nqp::if(
                  $value > 0,
                  nqp::if(
                    (my $pair := nqp::atkey($elems,$which)),
                    nqp::bindattr($pair,Pair,'$!value',$value),
                    nqp::bindkey($elems,$which,Pair.new($object,$value))
                  ),
                  nqp::deletekey($elems,$which)
                )
              )
          }
        )
    }

    multi method ASSIGN-KEY(BagHash:D: \k, \v) is raw {
        nqp::if(
          nqp::istype((my $value := v.Int),Failure),
          $value.throw,
          nqp::stmts(
            (my $object := self.OBJECTIFIER()(k)),
            nqp::if(
              $value > 0,
              nqp::bindkey($!elems,$object.WHICH,Pair.new($object,$value)),
              nqp::deletekey($!elems,$object.WHICH)
            )
          )
        );

        $value
    }

    multi method DELETE-KEY(BagHash:D: \k) {
        Rakudo::QuantHash.BAGGY-DELETE-KEY($!elems, self.WHICHIFY(k))
    }

    # https://github.com/rakudo/rakudo/issues/5057
    multi method deepmap(BagHash:D: &mapper) {
        self.WHAT.SETUP(
          Rakudo::QuantHash.BAGGY-MUTABLE-DEEPMAP($!elems, Int, &mapper, (* > 0))
        )
    }

#--- introspection methods
    method total() { Rakudo::QuantHash.BAG-TOTAL($!elems) }

#--- stringification methods

    multi method gist(BagHash:D: --> Str:D) {
        self.gistseen: self.^name, {
            nqp::concat(
              nqp::concat(
                nqp::concat(self.^name,'('),
                nqp::join(' ',
                  Rakudo::Sorting.MERGESORT-str(
                    Rakudo::QuantHash.RAW-VALUES-MAP(self, {
                        (my \value := nqp::getattr($_,Pair,'$!value')) == 1
                          ?? nqp::getattr($_,Pair,'$!key').gist
                          !! "{nqp::getattr($_,Pair,'$!key').gist}({value})"
                    })
                  )
                )
              ),
              ')',
            )
        }
    }

    multi method raku(BagHash:D \SELF: --> Str:D) {
        SELF.rakuseen: self.^name, {
            nqp::if(
              nqp::elems($!elems),
              nqp::stmts(
                (my \pairs := nqp::join(',',
                  Rakudo::QuantHash.RAW-VALUES-MAP(self, {
                      nqp::concat(
                        nqp::concat(
                          nqp::getattr($_,Pair,'$!key').raku,
                          '=>'
                        ),
                        nqp::getattr($_,Pair,'$!value').raku
                      )
                  })
                )),
                nqp::if(
                  nqp::eqaddr(self.keyof,Mu),
                  nqp::concat(
                    nqp::concat('(',pairs),
                    nqp::concat(').',self.^name)
                  ),
                  nqp::concat(
                    nqp::concat(self.^name,'.new-from-pairs('),
                    nqp::concat(pairs,')')
                  )
                )
              ),
              nqp::concat('().',self.^name)
            )
        }
    }

#--- coercion methods
    multi method Bag(BagHash:D: :view($)!) is implementation-detail {
        nqp::elems($!elems)
          ?? nqp::create(Bag).SETUP($!elems)
          !! bag()
    }
    multi method Bag(BagHash:D:) {
        nqp::elems($!elems)
          ?? Bag.SETUP(Rakudo::QuantHash.BAGGY-CLONE($!elems))
          !! bag()
    }
    multi method BagHash(BagHash:D:) { self }
    multi method Mix(BagHash:D:) {
        nqp::elems($!elems)
          ?? Mix.SETUP(Rakudo::QuantHash.BAGGY-CLONE($!elems))
          !! mix()
    }
    multi method MixHash(BagHash:D:) {
        MixHash.SETUP(Rakudo::QuantHash.BAGGY-CLONE($!elems))
    }

    multi method Setty(BagHash:U:) { SetHash      }
    multi method Setty(BagHash:D:) { self.SetHash }
    multi method Baggy(BagHash:U:) { BagHash      }
    multi method Baggy(BagHash:D:) { self         }
    multi method Mixy (BagHash:U:) { MixHash      }
    multi method Mixy (BagHash:D:) { self.MixHash }

    method clone() {
        self.WHAT.SETUP(Rakudo::QuantHash.BAGGY-CLONE($!elems))
    }

#--- iterator methods
    my sub proxy(str $which, \elems) is raw {
        # We are only sure that the key exists when the Proxy
        # is made, but we cannot be sure of its existence when
        # either the FETCH or STORE block is executed.  So we
        # still need to check for existence, and handle the case
        # where we need to (re-create) the key and value.  The
        # logic is therefore basically the same as in AT-KEY,
        # except we get the which key already without need to
        # process that.

        # save object for potential recreation
        my $object := nqp::atkey(elems,$which).key;

        Proxy.new(
          FETCH => {
              (my $pair := nqp::atkey(elems,$which))
                ?? $pair.value
                !! 0
          },
          STORE => -> $, Int() \value {
              nqp::if(
                # https://github.com/Raku/old-issue-tracker/issues/5567
                nqp::istype(value,Failure),
                value.throw,
                nqp::if(
                  value > 0,
                  nqp::if(
                    (my $pair := nqp::atkey(elems,$which)),
                    nqp::bindattr($pair,Pair,'$!value',nqp::decont(value)),
                    nqp::bindkey(
                      elems,$which,Pair.new($object,nqp::decont(value))
                    )
                  ),
                  nqp::deletekey(elems,$which)
                )
              )
          }
        )
    }

    multi method iterator(BagHash:D:) {
        Rakudo::QuantHash.BAGGY-MUTABLE-ITERATOR($!elems, &proxy)
    }
    multi method kv(BagHash:D:) {
        Seq.new(Rakudo::QuantHash.BAGGY-MUTABLE-KV($!elems, &proxy))
    }
    multi method values(BagHash:D:) {
        Seq.new(Rakudo::QuantHash.BAGGY-MUTABLE-VALUES($!elems, &proxy))
    }

#---- selection methods
    multi method grab(BagHash:D:) {
        nqp::elems($!elems)
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
        Rakudo::QuantHash.ADD-ITERATOR-TO-BAG(
          $!elems, to-add.iterator, self.OBJECTIFIER
        );
    }

    method remove(BagHash:D: \to-remove --> Nil) {
        Rakudo::QuantHash.SUB-ITERATOR-FROM-BAG(
          $!elems, to-remove.iterator, self.OBJECTIFIER
        )
    }
}

# vim: expandtab shiftwidth=4
