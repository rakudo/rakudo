# This file implements the following set operators:
#   (-)     set difference (ASCII)
#   ∖       set difference

proto sub infix:<(-)>(|) is pure {*}
multi sub infix:<(-)>()               { set() }
multi sub infix:<(-)>(QuantHash:D \a) { a     } # Set/Bag/Mix
multi sub infix:<(-)>(SetHash:D \a)   { a.Set }
multi sub infix:<(-)>(BagHash:D \a)   { a.Bag }
multi sub infix:<(-)>(MixHash:D \a)   { a.Mix }

multi sub infix:<(-)>(Setty:D \a, Setty:D \b) {
    (my $araw := a.RAW-HASH) && nqp::elems($araw)
      && (my $braw := b.RAW-HASH) && nqp::elems($braw)
      ?? nqp::create(a.Setty).SET-SELF(             # both have elems
           Rakudo::QuantHash.SUB-SET-FROM-SET($araw, $braw)
         )
      !! a                                          # no elems in a or b
}
multi sub infix:<(-)>(Setty:D \a, Map:D \b) {
    nqp::if(
      (my \araw := a.RAW-HASH) && nqp::elems(araw),
      nqp::create(a.Setty).SET-SELF(                      # elems in a
        nqp::if(
          nqp::elems(my \braw := nqp::getattr(nqp::decont(b),Map,'$!storage')),
          Rakudo::QuantHash.SUB-MAP-FROM-SET(araw, b),    # both have elems
          nqp::clone(araw)                                # no elems in b
        )
      ),
      a                                                   # no elems in a
    )
}
multi sub infix:<(-)>(Setty:D \a, Iterable:D \b) {
    nqp::if(
      (my $iterator := b.iterator).is-lazy,
      Set.fail-iterator-cannot-be-lazy('set difference'),
      nqp::if(
        (my $raw := a.RAW-HASH) && nqp::elems($raw),
        nqp::create(a.Setty).SET-SELF(                    # elems in b
          Rakudo::QuantHash.SUB-PAIRS-FROM-SET($raw, $iterator)
        ),
        a                                                 # no elems in b
      )
    )
}
multi sub infix:<(-)>(Mixy:D \a, Mixy:D \b) {    # needed as tie-breaker
    Rakudo::QuantHash.DIFFERENCE-MIXY-QUANTHASH(a, b)
}
multi sub infix:<(-)>(Mixy:D \a, QuantHash:D \b) {
    Rakudo::QuantHash.DIFFERENCE-MIXY-QUANTHASH(a, b)
}
multi sub infix:<(-)>(QuantHash:D \a, Mixy:D \b) {
    Rakudo::QuantHash.DIFFERENCE-MIXY-QUANTHASH(a.Mixy, b)
}
multi sub infix:<(-)>(Mixy:D \a, Map:D \b) {
    Rakudo::QuantHash.DIFFERENCE-MIXY-QUANTHASH(a, b.Set)
}
multi sub infix:<(-)>(Mixy:D \a, Any:D \b) {     # also Iterable
    Rakudo::QuantHash.DIFFERENCE-MIXY-QUANTHASH(a, b.Set)
}
multi sub infix:<(-)>(Any:D \a, Mixy:D \b) {
    Rakudo::QuantHash.DIFFERENCE-MIXY-QUANTHASH(a.Mix, b)
}
multi sub infix:<(-)>(Baggy:D \a, Mixy:D \b) {   # needed as tie-breaker
    Rakudo::QuantHash.DIFFERENCE-MIXY-QUANTHASH(a.Mixy, b)
}
multi sub infix:<(-)>(Baggy:D \a, Baggy:D \b) {  # needed as tie-breaker
    Rakudo::QuantHash.DIFFERENCE-BAGGY-QUANTHASH(a, b)
}
multi sub infix:<(-)>(Baggy:D \a, QuantHash:D \b) {
    Rakudo::QuantHash.DIFFERENCE-BAGGY-QUANTHASH(a, b)
}
multi sub infix:<(-)>(QuantHash:D \a, Baggy:D \b) {
    Rakudo::QuantHash.DIFFERENCE-BAGGY-QUANTHASH(a.Baggy, b)
}
multi sub infix:<(-)>(Baggy:D \a, Map:D \b) {
    Rakudo::QuantHash.DIFFERENCE-BAGGY-QUANTHASH(a, b.Bag)
}
multi sub infix:<(-)>(Baggy:D \a, Any:D \b) {    # also Iterable
    Rakudo::QuantHash.DIFFERENCE-BAGGY-QUANTHASH(a, b.Bag)
}
multi sub infix:<(-)>(Any \a, Baggy:D \b) {
    Rakudo::QuantHash.DIFFERENCE-BAGGY-QUANTHASH(a.Bag, b)
}
multi sub infix:<(-)>(Any \a, Map:D \b)      { infix:<(-)>(a.Set, b) }
multi sub infix:<(-)>(Any \a, Iterable:D \b) { infix:<(-)>(a.Set, b) }

multi sub infix:<(-)>(Any $, Failure:D \b) { b.throw }
multi sub infix:<(-)>(Failure:D \a, Any $) { a.throw }
multi sub infix:<(-)>(Any \a, Any \b) { infix:<(-)>(a.Set,b.Set) }

multi sub infix:<(-)>(+@p) {   # also Any

    sub subtract(Mu \elems, Mu \iter, \clone, \value --> Nil) {
        my $pair := nqp::ifnull(
          nqp::atkey(elems, nqp::iterkey_s(iter)),
          nqp::bindkey(
            elems,
            nqp::iterkey_s(iter),
            nqp::if(
              clone,
              nqp::p6bindattrinvres(
                nqp::clone(nqp::iterval(iter)),
                Pair,
                '$!value',
                0
              ),
              Pair.new(nqp::iterval(iter),0)
            )
          )
        );

        nqp::bindattr($pair,Pair,'$!value',
          nqp::getattr($pair,Pair,'$!value') - value
        );
    }

    nqp::if(
      (my $params := @p.iterator).is-lazy,
      Set.fail-iterator-cannot-be-lazy('set difference'),   # bye bye

      nqp::stmts(                                # fixed list of things to diff
        (my $type := nqp::if(
          nqp::istype((my $p := $params.pull-one),Mixy),
          Mix,
          nqp::if(nqp::istype($p,Baggy),Bag,Set)
        )),
        (my $mutable :=
             nqp::eqaddr($p.WHAT,MixHash)
          || nqp::eqaddr($p.WHAT,BagHash)
          || nqp::eqaddr($p.WHAT,SetHash)
        ),
        (my $elems := nqp::if(
          nqp::istype($p,Baggy),
          nqp::if(                               # already have a Baggy, clone
            (my $raw := $p.RAW-HASH),
            Rakudo::QuantHash.BAGGY-CLONE($raw),
            nqp::create(Rakudo::Internals::IterationSet)
          ),
          nqp::unless(                           # something else, Mix it!
            $p.Set.Mix.RAW-HASH,
            nqp::create(Rakudo::Internals::IterationSet)
          )
        )),

        nqp::until(
          nqp::eqaddr(($p := $params.pull-one),IterationEnd),

          nqp::if(                               # not done parsing
            nqp::istype($p,Baggy),

            nqp::stmts(                          # Mixy/Baggy semantics apply
              nqp::unless(                       # upgrade type if needed
                nqp::istype($type,Mix),
                ($type := nqp::if(nqp::istype($p,Mixy),Mix,Bag))
              ),
              nqp::if(
                ($raw := $p.RAW-HASH) && (my $iter := nqp::iterator($raw)),
                nqp::while(                      # something to process
                  $iter,
                  subtract(
                    $elems,
                    nqp::shift($iter),
                    1,
                    nqp::getattr(nqp::iterval($iter),Pair,'$!value')
                  )
                )
              )
            ),

            nqp::stmts(                          # not a Baggy/Mixy, assume Set
              ($raw := nqp::if(nqp::istype($p,Setty),$p,$p.Set).RAW-HASH)
                && ($iter := nqp::iterator($raw)),
              nqp::while(                        # something to process
                $iter,
                subtract($elems, nqp::shift($iter), 0, 1)
              )
            )
          )
        ),

        ($iter := nqp::iterator($elems)),        # start post-processing
        nqp::if(
          nqp::istype($type,Set),
          nqp::while(                            # need to create a Set
            $iter,
            nqp::if(
              nqp::getattr(nqp::iterval(nqp::shift($iter)),Pair,'$!value') > 0,
              nqp::bindkey(
                $elems,
                nqp::iterkey_s($iter),
                nqp::getattr(nqp::iterval($iter),Pair,'$!key')
              ),
              nqp::deletekey($elems,nqp::iterkey_s($iter))
            )
          ),
          nqp::if(
            nqp::istype($type,Mix),
            nqp::while(                          # convert to Mix semantics
              $iter,
              nqp::unless(
                nqp::getattr(nqp::iterval(nqp::shift($iter)),Pair,'$!value'),
                nqp::deletekey($elems,nqp::iterkey_s($iter))  # not valid in Mix
              )
            ),
            nqp::while(                          # convert to Bag semantics
              $iter,
              nqp::unless(
                nqp::getattr(nqp::iterval(nqp::shift($iter)),Pair,'$!value') >0,
                nqp::deletekey($elems,nqp::iterkey_s($iter))  # not valid in Bag
              )
            )
          )
        ),
        nqp::if(                                 # set to mutable if so started
          $mutable,
          $type := nqp::if(
            nqp::eqaddr($type,Mix),
            MixHash,
            nqp::if(nqp::eqaddr($type,Bag),BagHash,SetHash)
          )
        ),
        nqp::create($type).SET-SELF($elems)
      )
    )
}

# U+2216 SET MINUS
my constant &infix:<∖> := &infix:<(-)>;

# vim: expandtab shiftwidth=4
