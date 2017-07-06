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
    if Rakudo::Internals.ANY_DEFINED_TYPE(@p,Mixy) {
        my $mixhash = nqp::istype(@p[0], MixHash)
            ?? MixHash.new-from-pairs(@p.shift.pairs)
            !! @p.shift.MixHash;
        for @p.map(*.Mix(:view)) -> $mix {
            ($mixhash{$_} -= $mix{$_}) for $mix.keys;
        }
        $mixhash.DELETE-KEY($_) unless $mixhash.AT-KEY($_) for $mixhash.keys;
        $mixhash.Mix(:view);
    }
    elsif Rakudo::Internals.ANY_DEFINED_TYPE(@p,Baggy) {
        my $baghash = nqp::istype(@p[0], BagHash)
            ?? BagHash.new-from-pairs(@p.shift.pairs)
            !! @p.shift.BagHash;
        for @p.map(*.Bag(:view)) -> $bag {
            $bag{$_} < $baghash{$_}
              ?? ($baghash{$_} -= $bag{$_})
              !! $baghash.DELETE-KEY($_)
              for $baghash.keys;
        }
        $baghash.Bag(:view);
    }
    else {
        my $sethash = nqp::istype(@p[0],SetHash)
          ?? SetHash.new(@p.shift.keys)
          !! @p.shift.SetHash;
        for @p.map(*.Set(:view)) -> $set {
            $set{$_} && $sethash.DELETE-KEY($_) for $sethash.keys;
        }
        $sethash.Set(:view);
    }
}

# U+2216 SET MINUS
only sub infix:<∖>(|p) is pure {
    infix:<(-)>(|p);
}

# vim: ft=perl6 expandtab sw=4
