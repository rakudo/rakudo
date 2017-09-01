# This file implements the following set operators:
#   (-)     set difference (Texas)
#   ∖       set difference

proto sub infix:<(-)>(|) is pure { * }
multi sub infix:<(-)>()               { set()  }
multi sub infix:<(-)>(QuantHash:D $a) { $a     } # Set/Bag/Mix
multi sub infix:<(-)>(SetHash:D $a)   { $a.Set }
multi sub infix:<(-)>(BagHash:D $a)   { $a.Bag }
multi sub infix:<(-)>(MixHash:D $a)   { $a.Mix }
multi sub infix:<(-)>(Any $a)         { $a.Set } # also for Iterable/Map

multi sub infix:<(-)>(Setty:D $a, Setty:D $b) {
    nqp::if(
      (my $araw := $a.RAW-HASH) && nqp::elems($araw),
      nqp::if(                                 # elems in $a
        (my $braw := $b.RAW-HASH) && nqp::elems($braw),
        nqp::create(Set).SET-SELF(             # both have elems
          Rakudo::QuantHash.SUB-SET-FROM-SET($araw, $braw)
        ),
        $a.Set,                                # no elems in $b
      ),
      set()                                    # no elems in $a
    )
}
multi sub infix:<(-)>(Setty:D $a, Map:D $b) {
    nqp::if(
      (my $araw := $a.RAW-HASH) && nqp::elems($araw),
      nqp::create(Set).SET-SELF(                          # elems in $a
        nqp::if(
          (my $braw := nqp::getattr(nqp::decont($b),Map,'$!storage'))
            && nqp::elems($braw),
          Rakudo::QuantHash.SUB-MAP-FROM-SET($araw, $b),  # both have elems
          nqp::clone($araw)                               # no elems in $b
        )
      ),
      set()                                               # no elems in $a
    )
}
multi sub infix:<(-)>(Setty:D $a, Iterable:D $b) {
    nqp::if(
      (my $iterator := $b.iterator).is-lazy,
      Failure.new(X::Cannot::Lazy.new(:action('difference'),:what<set>)),
      nqp::if(
        (my $raw := $a.RAW-HASH) && nqp::elems($raw),
        nqp::create(Set).SET-SELF(
          Rakudo::QuantHash.SUB-PAIRS-FROM-SET($raw, $iterator)
        ),
        set()
      )
    )
}
multi sub infix:<(-)>(Mixy:D $a, Mixy:D $b) {    # needed as tie-breaker
    Rakudo::QuantHash.DIFFERENCE-MIXY-QUANTHASH($a, $b)
}
multi sub infix:<(-)>(Mixy:D $a, QuantHash:D $b) {
    Rakudo::QuantHash.DIFFERENCE-MIXY-QUANTHASH($a, $b)
}
multi sub infix:<(-)>(QuantHash:D $a, Mixy:D $b) {
    Rakudo::QuantHash.DIFFERENCE-MIXY-QUANTHASH($a.Mix, $b)
}
multi sub infix:<(-)>(Mixy:D $a, Map:D $b) {
    Rakudo::QuantHash.DIFFERENCE-MIXY-QUANTHASH($a, $b.Set)
}
multi sub infix:<(-)>(Mixy:D $a, Any:D $b) {     # also Iterable
    Rakudo::QuantHash.DIFFERENCE-MIXY-QUANTHASH($a, $b.Set)
}
multi sub infix:<(-)>(Any:D $a, Mixy:D $b) {
    Rakudo::QuantHash.DIFFERENCE-MIXY-QUANTHASH($a.Mix, $b)
}
multi sub infix:<(-)>(Baggy:D $a, Mixy:D $b) {   # needed as tie-breaker
    Rakudo::QuantHash.DIFFERENCE-MIXY-QUANTHASH($a.Mix, $b)
}
multi sub infix:<(-)>(Baggy:D $a, Baggy:D $b) {  # needed as tie-breaker
    Rakudo::QuantHash.DIFFERENCE-BAGGY-QUANTHASH($a, $b)
}
multi sub infix:<(-)>(Baggy:D $a, QuantHash:D $b) {
    Rakudo::QuantHash.DIFFERENCE-BAGGY-QUANTHASH($a, $b)
}
multi sub infix:<(-)>(QuantHash:D $a, Baggy:D $b) {
    Rakudo::QuantHash.DIFFERENCE-BAGGY-QUANTHASH($a.Bag, $b)
}
multi sub infix:<(-)>(Baggy:D $a, Map:D $b) {
    Rakudo::QuantHash.DIFFERENCE-BAGGY-QUANTHASH($a, $b.Set)
}
multi sub infix:<(-)>(Baggy:D $a, Any:D $b) {    # also Iterable
    Rakudo::QuantHash.DIFFERENCE-BAGGY-QUANTHASH($a, $b.Set)
}
multi sub infix:<(-)>(Any:D $a, Baggy:D $b) {
    Rakudo::QuantHash.DIFFERENCE-BAGGY-QUANTHASH($a.Bag, $b)
}
multi sub infix:<(-)>(Any:D $a, Map:D $b)      { infix:<(-)>($a.Set, $b) }
multi sub infix:<(-)>(Any:D $a, Iterable:D $b) { infix:<(-)>($a.Set, $b) }
multi sub infix:<(-)>(Any:D $a, Any:D $b)      { infix:<(-)>($a.Set, $b.Set) }

multi sub infix:<(-)>(**@p) {

    sub subtract(Mu \elems, Mu \iter, \clone, \value --> Nil) {
        nqp::stmts(
          (my $pair := nqp::ifnull(
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
          )),
          nqp::bindattr($pair,Pair,'$!value',
            nqp::getattr($pair,Pair,'$!value') - value
          )
        )
    }

    nqp::if(
      (my $params := @p.iterator).is-lazy,
      Failure.new(X::Cannot::Lazy.new(:action('difference'))),  # bye bye

      nqp::stmts(                                # fixed list of things to diff
        (my $type := nqp::if(
          nqp::istype((my $p := $params.pull-one),Mixy),
          Mix,
          nqp::if(nqp::istype($p,Baggy),Bag,Set)
        )),
        (my $elems := nqp::if(
          nqp::istype($p,Baggy),
          nqp::if(                               # already have a Baggy, clone
            (my $raw := $p.RAW-HASH),
            Rakudo::QuantHash.BAGGY-CLONE($raw),
            nqp::create(Rakudo::Internals::IterationSet)
          ),
          nqp::unless(                           # something else, Mix it!
            $p.Mix.RAW-HASH,
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
        nqp::create($type).SET-SELF($elems)
      )
    )
}

# U+2216 SET MINUS
my constant &infix:<∖> := &infix:<(-)>;

# vim: ft=perl6 expandtab sw=4
