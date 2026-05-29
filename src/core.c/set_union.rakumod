# This file implements the following set operators:
#   (|)     union (ASCII)
#   ∪       union

proto sub infix:<(|)>(|) is pure {*}
multi sub infix:<(|)>()               { set() }
multi sub infix:<(|)>(QuantHash:D $a) { $a    } # Set/Bag/Mix

multi sub infix:<(|)>(Setty:D $a, Setty:D $b) {
    $a.WHAT.SETUP(
      Rakudo::QuantHash.SET-UNION($a, $b)
    )
}
multi sub infix:<(|)>(Setty:D $a, Mixy:D  $b) { $a.Mixy  (|) $b }
multi sub infix:<(|)>(Setty:D $a, Baggy:D $b) { $a.Baggy (|) $b }

multi sub infix:<(|)>(Mixy:D $a, Mixy:D $b) {
    $a.WHAT.SETUP(
      Rakudo::QuantHash.BAGGY-UNION($a, $b)
    )
}
multi sub infix:<(|)>(Mixy:D $a, Baggy:D $b) {
    $a.WHAT.SETUP(
      Rakudo::QuantHash.BAGGY-UNION($a, $b)
    )
}
multi sub infix:<(|)>(Mixy:D $a, Setty:D $b) { $a (|) $b.Mix }

multi sub infix:<(|)>(Baggy:D $a, Mixy:D $b) { $a.Mixy (|) $b }
multi sub infix:<(|)>(Baggy:D $a, Baggy:D $b) {
    $a.WHAT.SETUP(
      Rakudo::QuantHash.BAGGY-UNION($a, $b)
    )
}
multi sub infix:<(|)>(Baggy:D $a, Setty:D $b) { $a (|) $b.Bag }

multi sub infix:<(|)>(Map:D \a, Map:D \b) {
    Set.SETUP(
      Rakudo::QuantHash.ADD-MAP-TO-SET(
        Rakudo::QuantHash.COERCE-MAP-TO-SET(a),
        b,
        Set.OBJECTIFIER
      )
    )
}

multi sub infix:<(|)>(Iterable:D \a, Iterable:D \b) {
    (my $aiterator := a.flat.iterator).is-lazy
      || (my $biterator := b.flat.iterator).is-lazy
      ?? Any.fail-iterator-cannot-be-lazy('union', 'set')
      !! Set.SETUP(
           Rakudo::QuantHash.ADD-PAIRS-TO-SET(
             Rakudo::QuantHash.ADD-PAIRS-TO-SET(
               nqp::create(Rakudo::Internals::IterationSet),
               $aiterator,
               Set.OBJECTIFIER
             ),
             $biterator,
             Set.OBJECTIFIER
           )
         )
}

multi sub infix:<(|)>(Failure:D $a, Any) { $a.throw }
multi sub infix:<(|)>(Any, Failure:D $b) { $b.throw }
multi sub infix:<(|)>(Any \a, Any \b) {
    nqp::isconcrete(a)
      ?? nqp::istype(a,Mixy)
        ?? a (|) b.Mix
        !! nqp::istype(a,Baggy)
          ?? a (|) b.Bag
          !! nqp::istype(a,Setty)
            ?? a (|) b.Set
            !! nqp::isconcrete(b)
              ?? nqp::istype(b,Mixy)
                ?? a.Mix (|) b
                !! nqp::istype(b,Baggy)
                  ?? a.Bag (|) b
                  !! a.Set (|) b.Set
              !! a (|) b.Set
      !! a.Set (|) b
}

multi sub infix:<(|)>(+@p) {   # also Any
    my $iterator := @p.iterator;
    nqp::if(
      nqp::eqaddr((my $result := $iterator.pull-one),IterationEnd),
      set(),          # nothing to process
      nqp::if(
        nqp::eqaddr((my $pulled := $iterator.pull-one),IterationEnd),
        $result.Set,  # only 1 elem to process
        nqp::stmts(
          nqp::repeat_until(
            nqp::eqaddr(($pulled := $iterator.pull-one),IterationEnd),
            ($result := $result (|) $pulled)
          ),
          $result
        )
      )
    )
}

# U+222A UNION
my constant &infix:<∪> := &infix:<(|)>;

# vim: expandtab shiftwidth=4
