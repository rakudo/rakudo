# This file implements the following set operators:
#   (+)     baggy addition (ASCII)
#   ⊎       baggy addition

proto sub infix:<(+)>(|) is pure {*}
multi sub infix:<(+)>()               { bag()  }
multi sub infix:<(+)>(Bag:D $a)       { $a     }
multi sub infix:<(+)>(Mix:D $a)       { $a     }
multi sub infix:<(+)>(MixHash:D $a)   { $a.Mix }

multi sub infix:<(+)>(Setty:D $a, Setty:D $b) {
    $a.WHAT.Baggy.SETUP(
      Rakudo::QuantHash.ADD-SET-TO-BAG(
        Rakudo::QuantHash.SET-BAGGIFY($a.RAW-HASH),
        $b.RAW-HASH,
        $a.OBJECTIFIER
      )
    )
}
multi sub infix:<(+)>(Setty:D $a, Mixy:D $b) {
    $a.WHAT.Mixy.SETUP(
      Rakudo::QuantHash.ADD-BAGGY-TO-MIX(
        Rakudo::QuantHash.SET-BAGGIFY($a.RAW-HASH),
        $b.RAW-HASH,
        $a.OBJECTIFIER
      )
    )
}
multi sub infix:<(+)>(Setty:D $a, Baggy:D $b) {
    $a.WHAT.Baggy.SETUP(
      Rakudo::QuantHash.ADD-BAG-TO-BAG(
        Rakudo::QuantHash.SET-BAGGIFY($a.RAW-HASH),
        $b.RAW-HASH,
        $a.OBJECTIFIER
      )
    )
}
multi sub infix:<(+)>(Setty:D $a, Map:D \b) {
    $a.WHAT.Baggy.SETUP(
      Rakudo::QuantHash.ADD-MAP-TO-BAG(
        Rakudo::QuantHash.SET-BAGGIFY($a.RAW-HASH), b, $a.OBJECTIFIER
      )
    )
}

multi sub infix:<(+)>(Mixy:D $a, Setty:D $b) {
    $a.WHAT.SETUP(
      Rakudo::QuantHash.ADD-SET-TO-MIX(
        Rakudo::QuantHash.BAGGY-CLONE($a.RAW-HASH),
        $b.RAW-HASH,
        $a.OBJECTIFIER
      )
    )
}

multi sub infix:<(+)>(Mixy:D $a, Baggy:D $b) {
    $a.WHAT.SETUP(
      Rakudo::QuantHash.ADD-BAGGY-TO-MIX(
        Rakudo::QuantHash.BAGGY-CLONE($a.RAW-HASH),
        $b.RAW-HASH,
        $a.OBJECTIFIER
      )
    )
}

multi sub infix:<(+)>(Baggy:D $a, QuantHash:D $b) {
    nqp::if(
      nqp::elems(my \araw := $a.RAW-HASH),
      nqp::if(                                         # elems on left
        (my \braw := $b.RAW-HASH) && nqp::elems(braw),
        nqp::stmts(                                    # elems on both sides
          (my \elems := Rakudo::QuantHash.BAGGY-CLONE(araw)),
          (my &objectifier := $a.OBJECTIFIER),
          nqp::if(
            nqp::istype($b,Mixy),
            nqp::if(nqp::istype($a,Bag),Mix,MixHash),
            nqp::if(nqp::istype($a,Bag),$a.WHAT,BagHash)
          ).SETUP(
            nqp::if(
              nqp::istype($b,Mixy),
              Rakudo::QuantHash.ADD-BAGGY-TO-MIX(elems, braw, &objectifier),
              nqp::if(
                nqp::istype($b,Baggy),
                Rakudo::QuantHash.ADD-BAG-TO-BAG(elems, braw, &objectifier),
                Rakudo::QuantHash.ADD-SET-TO-BAG(elems, braw, &objectifier)
              )
            )
          )
        ),
        nqp::if(nqp::istype($b,Mixy),$a.Mixy,$a)       # no elems on right
      ),
      nqp::if(                                         # no elems left/either
        nqp::istype($a,Mix) || nqp::istype($a,Bag),
        nqp::if( nqp::istype($b,Mixy),$b.Mix,    $b.Bag),
        nqp::if( nqp::istype($b,Mixy),$b.MixHash,$b.BagHash)
      )
    )
}

multi sub infix:<(+)>(Map:D \a, Map:D \b) {
    Bag.SETUP(
      Rakudo::QuantHash.ADD-MAP-TO-BAG(
        Rakudo::QuantHash.COERCE-MAP-TO-BAG(a),
        b,
        Bag.OBJECTIFIER
      )
    )
}

multi sub infix:<(+)>(Iterable:D \a, Iterable:D \b) {
    Bag.SETUP(
      Rakudo::QuantHash.ADD-PAIRS-TO-BAG(
        Rakudo::QuantHash.ADD-PAIRS-TO-BAG(
          nqp::create(Rakudo::Internals::IterationSet),
          a.iterator,
          Bag.OBJECTIFIER,
        ),
        b.iterator,
        Bag.OBJECTIFIER
      )
    )
}

multi sub infix:<(+)>(Any, Failure:D $b) { $b.throw }
multi sub infix:<(+)>(Failure:D $a, Any) { $a.throw }
multi sub infix:<(+)>(Any \a, Any \b) {
    nqp::if(
      nqp::istype(a,QuantHash) && nqp::isconcrete(a),
      nqp::if(
        nqp::istype(a,Mixy) || nqp::istype(b,Mixy),
        infix:<(+)>(a.Mixy,  b.Mix(:view)),  # :view is implementation-detail
        infix:<(+)>(a.Baggy, b.Bag(:view))   # :view is implementation-detail
      ),
      nqp::if(
        nqp::istype(a,Mixy) || nqp::istype(b,Mixy),
        infix:<(+)>(a.Mix, b.Mix(:view)),    # :view is implementation-detail
        infix:<(+)>(a.Bag, b.Bag(:view))     # :view is implementation-detail
      )
    )
}

multi sub infix:<(+)>(+@p) {    # also Any
    my $iterator := @p.iterator;
    nqp::if(
      nqp::eqaddr((my $result := $iterator.pull-one),IterationEnd),
      bag(),          # nothing to process
      nqp::if(
        nqp::eqaddr((my $pulled := $iterator.pull-one),IterationEnd),
        $result.Bag,  # only 1 elem to process
        nqp::stmts(
          nqp::repeat_until(
            nqp::eqaddr(($pulled := $iterator.pull-one),IterationEnd),
            ($result := $result (+) $pulled)
          ),
          $result
        )
      )
    )
}

# U+228E MULTISET UNION
my constant &infix:<⊎> := &infix:<(+)>;

# vim: expandtab shiftwidth=4
