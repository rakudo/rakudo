# This file implements the following set operators:
#   (|)     union (Texas)
#   ∪       union

proto sub infix:<(|)>(|) is pure { * }
multi sub infix:<(|)>()               { set()  }
multi sub infix:<(|)>(QuantHash:D $a) { $a     } # Set/Bag/Mix
multi sub infix:<(|)>(SetHash:D $a)   { $a.Set }
multi sub infix:<(|)>(BagHash:D $a)   { $a.Bag }
multi sub infix:<(|)>(MixHash:D $a)   { $a.Mix }
multi sub infix:<(|)>(Any $a)         { $a.Set } # also for Iterable/Map

multi sub infix:<(|)>(Setty:D $a, Setty:D $b) {
    nqp::if(
      (my $araw := $a.RAW-HASH) && nqp::elems($araw),
      nqp::if(                                    # first has elems
        (my $braw := $b.RAW-HASH) && nqp::elems($braw),
        nqp::stmts(                               # second has elems
          (my $elems := nqp::clone($araw)),
          (my $iter := nqp::iterator($braw)),
          nqp::while(                             # loop over keys of second
            $iter,
            nqp::bindkey(                         # bind into clone of first
              $elems,
              nqp::iterkey_s(nqp::shift($iter)),
              nqp::iterval($iter)
            )
          ),
          nqp::create(Set).SET-SELF($elems)       # make it a Set
        ),
        $a.Set                                    # no second, so first
      ),
      nqp::if(                                    # no first
        ($braw := $b.RAW-HASH) && nqp::elems($braw),
        $b.Set,                                   # but second
        set()                                     # both empty
      )
    )
}

multi sub infix:<(|)>(Mixy:D $a, Mixy:D $b) {
    nqp::if(
      (my $araw := $a.RAW-HASH) && nqp::elems($araw),
      nqp::if(                                    # first has elems
        (my $braw := $b.RAW-HASH) && nqp::elems($braw),
        nqp::stmts(                               # second has elems
          (my $elems := nqp::clone($araw)),
          (my $iter := nqp::iterator($braw)),
          nqp::while(                             # loop over keys of second
            $iter,
            nqp::if(
              nqp::existskey(
                $araw,
                (my $key := nqp::iterkey_s(nqp::shift($iter)))
              ),
              nqp::if(   # must use HLL < because values can be bignums
                nqp::getattr(
                  nqp::decont(nqp::atkey($araw,$key)),Pair,'$!value')
                < nqp::getattr(
                    nqp::decont(nqp::atkey($braw,$key)),Pair,'$!value'),
                nqp::bindkey($elems,$key,nqp::atkey($braw,$key))
              ),
              nqp::bindkey($elems,$key,nqp::atkey($braw,$key))
            )
          ),
          nqp::create(Mix).SET-SELF($elems)       # make it a Mix
        ),
        $a.Mix                                    # no second, so first
      ),
      nqp::if(                                    # no first
        ($braw := $b.RAW-HASH) && nqp::elems($braw),
        $b.Mix,                                   # but second
        mix()                                     # both empty
      )
    )
}

multi sub infix:<(|)>(Mixy:D $a, Baggy:D $b) { infix:<(|)>($a, $b.Mix) }
multi sub infix:<(|)>(Baggy:D $a, Mixy:D $b) { infix:<(|)>($a.Mix, $b) }
multi sub infix:<(|)>(Baggy:D $a, Baggy:D $b) {
    nqp::if(
      (my $araw := $a.RAW-HASH) && nqp::elems($araw),
      nqp::if(                                    # first has elems
        (my $braw := $b.RAW-HASH) && nqp::elems($braw),
        nqp::stmts(                               # second has elems
          (my $elems := nqp::clone($araw)),
          (my $iter := nqp::iterator($braw)),
          nqp::while(                             # loop over keys of second
            $iter,
            nqp::if(
              nqp::existskey(
                $araw,
                (my $key := nqp::iterkey_s(nqp::shift($iter)))
              ),
              nqp::if(
                nqp::islt_i(
                  nqp::getattr(
                    nqp::decont(nqp::atkey($araw,$key)),Pair,'$!value'),
                  nqp::getattr(
                    nqp::decont(nqp::atkey($braw,$key)),Pair,'$!value')
                ),
                nqp::bindkey($elems,$key,nqp::atkey($braw,$key))
              ),
              nqp::bindkey($elems,$key,nqp::atkey($braw,$key))
            )
          ),
          nqp::create(Bag).SET-SELF($elems)       # make it a Bag
        ),
        $a.Bag                                    # no second, so first
      ),
      nqp::if(                                    # no first
        ($braw := $b.RAW-HASH) && nqp::elems($braw),
        $b.Bag,                                   # but second
        bag()                                     # both empty
      )
    )
}

multi sub infix:<(|)>(Map:D $a, Map:D $b) {
    nqp::create(Set).SET-SELF(
      Rakudo::QuantHash.ADD-MAP-TO-SET(
        Rakudo::QuantHash.COERCE-MAP-TO-SET($a),
        $b
      )
    )
}

multi sub infix:<(|)>(Iterable:D $a, Iterable:D $b) {
    nqp::if(
      (my $aiterator := $a.flat.iterator).is-lazy
        || (my $biterator := $b.flat.iterator).is-lazy,
      Failure.new(X::Cannot::Lazy.new(:action<union>,:what<set>)),
      nqp::create(Set).SET-SELF(
        Rakudo::QuantHash.ADD-PAIRS-TO-SET(
          Rakudo::QuantHash.ADD-PAIRS-TO-SET(
            nqp::create(Rakudo::Internals::IterationSet),
            $aiterator
          ),
          $biterator
        )
      )
    )
}
multi sub infix:<(|)>(**@p) {
    if Rakudo::Internals.ANY_DEFINED_TYPE(@p, Mixy) {
        my $mixhash = nqp::istype(@p[0], MixHash)
            ?? MixHash.new-from-pairs(@p.shift.pairs)
            !! @p.shift.MixHash;
        for @p.map(*.Mix(:view)) -> $mix {
            for $mix.keys {
                # Handle negative weights: don't take max for keys that are zero
                $mixhash{$_} ?? ($mixhash{$_} max= $mix{$_})
                             !!  $mixhash{$_}    = $mix{$_}
            }
        }
        $mixhash.Mix(:view);
    }
    elsif Rakudo::Internals.ANY_DEFINED_TYPE(@p, Baggy) {
        my $baghash = nqp::istype(@p[0], BagHash)
            ?? BagHash.new-from-pairs(@p.shift.pairs)
            !! @p.shift.BagHash;
        for @p.map(*.Bag(:view)) -> $bag {
            $baghash{$_} max= $bag{$_} for $bag.keys;
        }
        $baghash.Bag(:view);
    }
    else {
        Set.new( @p.map(*.Set(:view).keys.Slip) );
    }
}
# U+222A UNION
my constant &infix:<∪> := &infix:<(|)>;

# vim: ft=perl6 expandtab sw=4
