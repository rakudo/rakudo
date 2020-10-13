# This file implements the following set operators:
#   (+)     baggy addition (ASCII)
#   ⊎       baggy addition

proto sub infix:<(+)>(|) is pure {*}
multi sub infix:<(+)>()               { bag() }
multi sub infix:<(+)>(Bag:D \a)       { a     }
multi sub infix:<(+)>(Mix:D \a)       { a     }
multi sub infix:<(+)>(MixHash:D \a)   { a.Mix }

multi sub infix:<(+)>(Setty:D \a, QuantHash:D \b) {
    nqp::if(
      (my \araw := a.RAW-HASH) && nqp::elems(araw),
      nqp::if(                                         # elems on left
        (my \braw := b.RAW-HASH) && nqp::elems(braw),
        nqp::stmts(                                    # elems on both sides
          (my \elems := Rakudo::QuantHash.SET-BAGGIFY(araw)),
          nqp::create(
            nqp::if( nqp::istype(b,Mixy), a.WHAT.Mixy, a.WHAT.Baggy )
          ).SET-SELF(
            nqp::if(
              nqp::istype(b,Mixy),
              Rakudo::QuantHash.ADD-MIX-TO-MIX(elems, braw),
              nqp::if(
                nqp::istype(b,Baggy),
                Rakudo::QuantHash.ADD-BAG-TO-BAG(elems, braw),
                Rakudo::QuantHash.ADD-SET-TO-BAG(elems, braw)
              )
            )
          )
        ),
        nqp::if(nqp::istype(b,Mixy),a.Mixy,a.Baggy)    # no elems on right
      ),
      nqp::if(                                         # no elems left/either
        nqp::istype(a,Set),
        nqp::if( nqp::istype(b,Mixy), b.Mix,     b.Bag),
        nqp::if( nqp::istype(b,Mixy), b.MixHash, b.BagHash)
      )
    )
}
multi sub infix:<(+)>(Setty:D \a, Map:D \b) {
    nqp::if(
      (my \araw := a.RAW-HASH) && nqp::elems(araw),
      nqp::if(                                         # elems on left
        nqp::elems(nqp::getattr(nqp::decont(b),Map,'$!storage')),
        nqp::create(
          nqp::if( nqp::istype(a,Set), Bag, BagHash )
        ).SET-SELF(                                    # elems on both sides
          Rakudo::QuantHash.ADD-MAP-TO-BAG(
            Rakudo::QuantHash.SET-BAGGIFY(araw), b
          )
        ),
        a.Baggy                                        # no elems on right
      ),
      nqp::if( nqp::istype(a,Set),b.Bag,b.BagHash )    # no elems left/either
    )
}
multi sub infix:<(+)>(Mixy:D \a, QuantHash:D \b) {
    nqp::if(
      (my \araw := a.RAW-HASH) && nqp::elems(araw),
      nqp::if(                                         # elems on left
        (my \braw := b.RAW-HASH) && nqp::elems(braw),
        nqp::stmts(                                    # elems on both sides
          (my \elems := Rakudo::QuantHash.BAGGY-CLONE(araw)),
          nqp::create(a.WHAT).SET-SELF(
            nqp::if(
              nqp::istype(b,Baggy),
              Rakudo::QuantHash.ADD-MIX-TO-MIX(elems, braw),
              Rakudo::QuantHash.ADD-SET-TO-MIX(elems, braw)
            )
          )
        ),
        a                                              # no elems on right
      ),
      nqp::if( nqp::istype(a,Mix),b.Mix,b.MixHash )    # no elems left/either
    )
}

multi sub infix:<(+)>(Baggy:D \a, QuantHash:D \b) {
    nqp::if(
      (my \araw := a.RAW-HASH) && nqp::elems(araw),
      nqp::if(                                         # elems on left
        (my \braw := b.RAW-HASH) && nqp::elems(braw),
        nqp::stmts(                                    # elems on both sides
          (my \elems := Rakudo::QuantHash.BAGGY-CLONE(araw)),
          nqp::create(
            nqp::if(
              nqp::istype(b,Mixy),
              nqp::if( nqp::istype(a,Bag), Mix, MixHash ),
              nqp::if( nqp::istype(a,Bag), Bag, BagHash )
            )
          ).SET-SELF(
            nqp::if(
              nqp::istype(b,Mixy),
              Rakudo::QuantHash.ADD-MIX-TO-MIX(elems, braw),
              nqp::if(
                nqp::istype(b,Baggy),
                Rakudo::QuantHash.ADD-BAG-TO-BAG(elems, braw),
                Rakudo::QuantHash.ADD-SET-TO-BAG(elems, braw)
              )
            )
          )
        ),
        nqp::if(nqp::istype(b,Mixy),a.Mixy,a)          # no elems on right
      ),
      nqp::if(                                         # no elems left/either
        nqp::istype(a,Mix) || nqp::istype(a,Bag),
        nqp::if( nqp::istype(b,Mixy), b.Mix,     b.Bag),
        nqp::if( nqp::istype(b,Mixy), b.MixHash, b.BagHash)
      )
    )
}

multi sub infix:<(+)>(Map:D \a, Map:D \b) {
    nqp::if(
      nqp::elems(nqp::getattr(nqp::decont(a),Map,'$!storage')),
      nqp::if(                                         # elems on left
        nqp::elems(nqp::getattr(nqp::decont(b),Map,'$!storage')),
        nqp::create(Bag).SET-SELF(                     # elems on both sides
          Rakudo::QuantHash.ADD-MAP-TO-BAG(
            Rakudo::QuantHash.COERCE-MAP-TO-BAG(a), b
          )
        ),
        a.Bag                                          # no elems on right
      ),
      b.Bag                                            # no elems left/either
    )
}

multi sub infix:<(+)>(Iterable:D \a, Iterable:D \b) {
    nqp::create(Bag).SET-SELF(
      Rakudo::QuantHash.ADD-PAIRS-TO-BAG(
        Rakudo::QuantHash.ADD-PAIRS-TO-BAG(
          nqp::create(Rakudo::Internals::IterationSet),
          a.iterator,
          Mu
        ),
        b.iterator,
        Mu
      )
    )
}

multi sub infix:<(+)>(Any $, Failure:D \b) { b.throw }
multi sub infix:<(+)>(Failure:D \a, Any $) { a.throw }
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
    my $result := @p.shift;
    if @p {
        $result := $result (+) @p.shift while @p;
        $result
    }
    else {
        $result.Bag
    }
}

# U+228E MULTISET UNION
my constant &infix:<⊎> := &infix:<(+)>;

# vim: expandtab shiftwidth=4
