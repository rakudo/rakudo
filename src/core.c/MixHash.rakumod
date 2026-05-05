my class MixHash does Mixy {

    method ^parameterize(Mu \base, Mu \type) {
        my \what := base.^mixin(QuantHash::KeyOf[type]);
        what.^set_name(
          nqp::concat(base.^name,'[') ~ nqp::concat(type.^name,']')
        );
        what
    }

#--- interface methods
    method total() { Rakudo::QuantHash.MIX-TOTAL($!elems) }
    method !total-positive() { Rakudo::QuantHash.MIX-TOTAL-POSITIVE($!elems) }

    multi method STORE(MixHash:D: Any:D \keys --> MixHash:D) {
        (my \iterator := keys.iterator).is-lazy
          ?? self.fail-iterator-cannot-be-lazy('initialize')
          !! self.SETUP(
               Rakudo::QuantHash.ADD-PAIRS-TO-MIX(
                 nqp::create(Rakudo::Internals::IterationSet),
                 iterator,
                 self.OBJECTIFIER
               )
             )
    }
    multi method STORE(MixHash:D: \objects, \values --> MixHash:D) {
        self.SETUP(
          Rakudo::QuantHash.ADD-OBJECTS-VALUES-TO-MIX(
            nqp::create(Rakudo::Internals::IterationSet),
            objects.iterator,
            values.iterator,
            self.OBJECTIFIER
          )
        )
    }
    multi method AT-KEY(MixHash:D: \k) is raw {
        Proxy.new(
          FETCH => {
              (my $pair := nqp::atkey($!elems,self.WHICHIFY(k)))
                ?? $pair.value
                !! 0
          },
          STORE => -> $, Real() $value {
              my     $elems  := $!elems;
              my     $object := self.OBJECTIFIER()(k);
              my str $which   = $object.WHICH;

              nqp::if(
                # https://github.com/Raku/old-issue-tracker/issues/5567
                nqp::istype($value,Failure),
                $value.throw,
                nqp::if(
                  $value == 0,
                  nqp::deletekey($elems,$which),
                  nqp::if(
                    (my $pair := nqp::atkey($elems,$which)),
                    nqp::bindattr($pair,Pair,'$!value',$value),
                    nqp::bindkey($elems,$which,Pair.new($object,$value))
                  )
                )
              )
          }
        )
    }

    multi method ASSIGN-KEY(MixHash:D: \k, \v) is raw {
        nqp::if(
          nqp::istype((my $value := v.Real),Failure),
          $value.throw,
          nqp::stmts(
            (my $object := self.OBJECTIFIER()(k)),
            nqp::if(
              $value == 0,
              nqp::deletekey($!elems,$object.WHICH),
              nqp::bindkey($!elems,$object.WHICH,Pair.new($object,$value))
            )
          )
        );

        $value
    }

    multi method DELETE-KEY(MixHash:D: \k) {
        Rakudo::QuantHash.BAGGY-DELETE-KEY($!elems, self.WHICHIFY(k))
    }

    # https://github.com/rakudo/rakudo/issues/5057
    multi method deepmap(MixHash:D: &mapper) {
        self.WHAT.SETUP(
          Rakudo::QuantHash.BAGGY-MUTABLE-DEEPMAP($!elems, Real, &mapper, (* != 0))
        )
    }

#--- stringification methods

    multi method gist(MixHash:D: --> Str:D) {
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

    multi method raku(MixHash:D \SELF: --> Str:D) {
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
    multi method Mix(MixHash:D: :view($)!) is implementation-detail {
        nqp::elems($!elems) ?? Mix.SETUP($!elems) !! mix()
    }
    multi method Mix(MixHash:D:) {
        nqp::elems($!elems)
          ?? Mix.SETUP(Rakudo::QuantHash.BAGGY-CLONE($!elems))
          !! mix()
    }
    multi method MixHash(MixHash:D:) { self }

    multi method Setty(MixHash:U:) { SetHash      }
    multi method Setty(MixHash:D:) { self.SetHash }
    multi method Baggy(MixHash:U:) { BagHash      }
    multi method Baggy(MixHash:D:) { self.BagHash }
    multi method Mixy (MixHash:U:) { MixHash      }
    multi method Mixy (MixHash:D:) { self         }

    method clone() {
        self.WHAT.SETUP(Rakudo::QuantHash.BAGGY-CLONE($!elems))
    }

#--- iterator methods
    my sub proxy(str $which, Mu \elems) is raw {
        # We are only sure that the key exists when the Proxy
        # is made, but we cannot be sure of its existence when
        # either the FETCH or STORE block is executed.  So we
        # still need to check for existence, and handle the case
        # where we need to (re-create) the key and value.  The
        # logic is therefore basically the same as in AT-KEY,
        # except for tests for allocated storage and .WHICH
        # processing.

        # save object for potential recreation
        my $object := nqp::atkey(elems,$which).key;

        Proxy.new(
          FETCH => {
              (my $pair := nqp::atkey(elems,$which))
                ?? $pair.value
                !! 0
          },
          STORE => -> $, Real() \value {
              nqp::if(
                # https://github.com/Raku/old-issue-tracker/issues/5567
                nqp::istype(value,Failure),
                value.throw,
                nqp::if(
                  value == 0,
                  nqp::deletekey(elems,$which),
                  nqp::if(
                    (my $pair := nqp::atkey(elems,$which)),
                    nqp::bindattr($pair,Pair,'$!value',nqp::decont(value)),
                    nqp::bindkey(
                      elems,$which,Pair.new($object,nqp::decont(value))
                    )
                  )
                )
              )
          }
        )
    }

    multi method iterator(MixHash:D:) {
        Rakudo::QuantHash.BAGGY-MUTABLE-ITERATOR($!elems, &proxy)
    }
    multi method kv(MixHash:D:) {
        Seq.new(Rakudo::QuantHash.BAGGY-MUTABLE-KV($!elems, &proxy))
    }
    multi method values(MixHash:D:) {
        Seq.new(Rakudo::QuantHash.BAGGY-MUTABLE-VALUES($!elems, &proxy))
    }
}

# vim: expandtab shiftwidth=4
