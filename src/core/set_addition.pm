# This file implements the following set operators:
#   (+)     baggy addition (ASCII)
#   ⊎       baggy addition

proto sub infix:<(+)>(|) is pure { * }
multi sub infix:<(+)>()               { bag()  }
multi sub infix:<(+)>(Bag:D $a)       { $a     }
multi sub infix:<(+)>(Mix:D $a)       { $a     }
multi sub infix:<(+)>(MixHash:D $a)   { $a.Mix }
multi sub infix:<(+)>(Any $a)         { $a.Bag }

multi sub infix:<(+)>(Setty:D $a, QuantHash:D $b) {
    nqp::if(
      (my $araw := $a.RAW-HASH) && nqp::elems($araw),
      nqp::if(                                         # elems on left
        (my $braw := $b.RAW-HASH) && nqp::elems($braw),
        nqp::stmts(                                    # elems on both sides
          (my $elems := Rakudo::QuantHash.SET-BAGGIFY($araw)),
          nqp::create(nqp::if(nqp::istype($b,Mixy),Mix,Bag)).SET-SELF(
            nqp::if(
              nqp::istype($b,Mixy),
              Rakudo::QuantHash.ADD-MIX-TO-MIX($elems, $braw),
              nqp::if(
                nqp::istype($b,Baggy),
                Rakudo::QuantHash.ADD-BAG-TO-BAG($elems, $braw),
                Rakudo::QuantHash.ADD-SET-TO-BAG($elems, $braw)
              )
            )
          )
        ),
        nqp::if(nqp::istype($b,Mixy),$a.Mix,$a.Bag)    # no elems on right
      ),
      nqp::if(nqp::istype($b,Mixy),$b.Mix,$b.Bag)      # no elems left/either
    )
}
multi sub infix:<(+)>(Setty:D $a, Map:D $b) {
    nqp::if(
      (my $araw := $a.RAW-HASH) && nqp::elems($araw),
      nqp::if(                                         # elems on left
        (my $braw := nqp::getattr(nqp::decont($b),Map,'$!storage'))
          && nqp::elems($braw),
        nqp::create(Bag).SET-SELF(                     # elems on both sides
          Rakudo::QuantHash.ADD-MAP-TO-BAG(
            Rakudo::QuantHash.SET-BAGGIFY($araw), $b
          )
        ),
        $a.Bag                                         # no elems on right
      ),
      $b.Bag                                           # no elems left/either
    )
}
multi sub infix:<(+)>(Mixy:D $a, QuantHash:D $b) {
    nqp::if(
      (my $araw := $a.RAW-HASH) && nqp::elems($araw),
      nqp::if(                                         # elems on left
        (my $braw := $b.RAW-HASH) && nqp::elems($braw),
        nqp::stmts(                                    # elems on both sides
          (my $elems := Rakudo::QuantHash.BAGGY-CLONE($araw)),
          nqp::create(Mix).SET-SELF(
            nqp::if(
              nqp::istype($b,Baggy),
              Rakudo::QuantHash.ADD-MIX-TO-MIX($elems, $braw),
              Rakudo::QuantHash.ADD-SET-TO-MIX($elems, $braw)
            )
          )
        ),
        $a.Mix                                         # no elems on right
      ),
      $b.Mix                                           # no elems left/either
    )
}

multi sub infix:<(+)>(Baggy:D $a, QuantHash:D $b) {
    nqp::if(
      (my $araw := $a.RAW-HASH) && nqp::elems($araw),
      nqp::if(                                         # elems on left
        (my $braw := $b.RAW-HASH) && nqp::elems($braw),
        nqp::stmts(                                    # elems on both sides
          (my $elems := Rakudo::QuantHash.BAGGY-CLONE($araw)),
          nqp::create(nqp::if(nqp::istype($b,Mixy),Mix,Bag)).SET-SELF(
            nqp::if(
              nqp::istype($b,Mixy),
              Rakudo::QuantHash.ADD-MIX-TO-MIX($elems, $braw),
              nqp::if(
                nqp::istype($b,Baggy),
                Rakudo::QuantHash.ADD-BAG-TO-BAG($elems, $braw),
                Rakudo::QuantHash.ADD-SET-TO-BAG($elems, $braw)
              )
            )
          )
        ),
        nqp::if(nqp::istype($b,Mixy),$a.Mix,$a.Bag)    # no elems on right
      ),
      nqp::if(nqp::istype($b,Mixy),$b.Mix,$b.Bag)      # no elems left/either
    )
}

multi sub infix:<(+)>(Map:D $a, Map:D $b) {
    nqp::if(
      (my $araw := nqp::getattr(nqp::decont($b),Map,'$!storage'))
        && nqp::elems($araw),
      nqp::if(                                         # elems on left
        (my $braw := nqp::getattr(nqp::decont($b),Map,'$!storage'))
          && nqp::elems($braw),
        nqp::create(Bag).SET-SELF(                     # elems on both sides
          Rakudo::QuantHash.ADD-MAP-TO-BAG(
            Rakudo::QuantHash.COERCE-MAP-TO-BAG($a), $b
          )
        ),
        $a.Bag                                         # no elems on right
      ),
      $b.Bag                                           # no elems left/either
    )
}

multi sub infix:<(+)>(Iterable:D $a, Iterable:D $b) {
    nqp::create(Bag).SET-SELF(
      Rakudo::QuantHash.ADD-PAIRS-TO-BAG(
        Rakudo::QuantHash.ADD-PAIRS-TO-BAG(
          nqp::create(Rakudo::Internals::IterationSet),
          $a.iterator
        ),
        $b.iterator
      )
    )
}

multi sub infix:<(+)>(Any $a, Any $b) {
    nqp::if(
      nqp::istype($a,Baggy:D),
      infix:<(+)>($a, $b.Bag),
      nqp::if(
        nqp::istype($b,Baggy:D),
        infix:<(+)>($a.Bag, $b),
        infix:<(+)>($a.Bag, $b.Bag)
      )
    )
}

multi sub infix:<(+)>(**@p) {
    my $result = @p.shift;
    $result = $result (+) @p.shift while @p;
    $result
}

# U+228E MULTISET UNION
my constant &infix:<⊎> := &infix:<(+)>;

# vim: ft=perl6 expandtab sw=4
