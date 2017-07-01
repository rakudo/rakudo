proto sub infix:<(elem)>($, $ --> Bool:D) is pure {*}
multi sub infix:<(elem)>(Str:D $a, Map:D $b --> Bool:D) {
    nqp::p6bool($b.AT-KEY($a))
}
multi sub infix:<(elem)>(Any $a, Map:D $b --> Bool:D) {
    nqp::p6bool(
      (my $storage := nqp::getattr(nqp::decont($b),Map,'$!storage'))
        && nqp::elems($storage)                         # haz a haystack
        && nqp::not_i(nqp::eqaddr($b.keyof,Str(Any)))   # is object hash
        && nqp::getattr(
             nqp::ifnull(
               nqp::atkey($storage,$a.WHICH),           # exists
               BEGIN   # provide virtual value False    # did not exist
                 nqp::p6bindattrinvres(nqp::create(Pair),Pair,'$!value',False)
             ),
             Pair,
             '$!value'
           )
    )
}
multi sub infix:<(elem)>(Any $a, Iterable:D $b --> Bool:D) {
    nqp::if(
      (my $iterator := $b.iterator).is-lazy,
      Failure.new(X::Cannot::Lazy.new(:action<(elem)>)),
      nqp::stmts(
        (my str $needle = $a.WHICH),
        nqp::until(
          nqp::eqaddr((my $pulled := $iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::iseq_s($needle,$pulled.WHICH),
            return True
          )
        ),
        False
      )
    )
}
multi sub infix:<(elem)>(Any $a, QuantHash:D $b --> Bool:D) {
    nqp::p6bool(
      (my $elems := $b.raw_hash) && nqp::existskey($elems,$a.WHICH)
    )
}
multi sub infix:<(elem)>(Any $a, Any $b --> Bool:D) {
    $a (elem) $b.Set(:view);
}
# U+2208 ELEMENT OF
only sub infix:<∈>($a, $b --> Bool:D) is pure {
    $a (elem) $b;
}
# U+2209 NOT AN ELEMENT OF
only sub infix:<∉>($a, $b --> Bool:D) is pure {
    not $a (elem) $b;
}

only sub infix:<(cont)>($a, $b --> Bool:D) is pure { $b (elem) $a }

# U+220B CONTAINS AS MEMBER
only sub infix:<∋>($a, $b --> Bool:D) is pure {
    $b (elem) $a;
}
# U+220C DOES NOT CONTAIN AS MEMBER
only sub infix:<∌>($a, $b --> Bool:D) is pure {
    not $b (elem) $a;
}

proto sub infix:<(|)>(|) is pure { * }
multi sub infix:<(|)>()               { set()  }
multi sub infix:<(|)>(QuantHash:D $a) { $a     } # Set/Bag/Mix
multi sub infix:<(|)>(SetHash:D $a)   { $a.Set }
multi sub infix:<(|)>(BagHash:D $a)   { $a.Bag }
multi sub infix:<(|)>(MixHash:D $a)   { $a.Mix }
multi sub infix:<(|)>(Any $a)         { $a.Set } # also for Iterable/Map

multi sub infix:<(|)>(Setty:D $a, Setty:D $b) {
    nqp::if(
      (my $araw := $a.raw_hash) && nqp::elems($araw),
      nqp::if(                                    # first has elems
        (my $braw := $b.raw_hash) && nqp::elems($braw),
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
        ($braw := $b.raw_hash) && nqp::elems($braw),
        $b.Set,                                   # but second
        set()                                     # both empty
      )
    )
}

multi sub infix:<(|)>(Mixy:D $a, Mixy:D $b) {
    nqp::if(
      (my $araw := $a.raw_hash) && nqp::elems($araw),
      nqp::if(                                    # first has elems
        (my $braw := $b.raw_hash) && nqp::elems($braw),
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
        ($braw := $b.raw_hash) && nqp::elems($braw),
        $b.Mix,                                   # but second
        mix()                                     # both empty
      )
    )
}

multi sub infix:<(|)>(Mixy:D $a, Baggy:D $b) { infix:<(|)>($a, $b.Mix) }
multi sub infix:<(|)>(Baggy:D $a, Mixy:D $b) { infix:<(|)>($a.Mix, $b) }
multi sub infix:<(|)>(Baggy:D $a, Baggy:D $b) {
    nqp::if(
      (my $araw := $a.raw_hash) && nqp::elems($araw),
      nqp::if(                                    # first has elems
        (my $braw := $b.raw_hash) && nqp::elems($braw),
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
        ($braw := $b.raw_hash) && nqp::elems($braw),
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
only sub infix:<∪>(|p) is pure {
    infix:<(|)>(|p);
}

proto sub infix:<(&)>(|) is pure { * }
multi sub infix:<(&)>()               { set()  }
multi sub infix:<(&)>(QuantHash:D $a) { $a     } # Set/Bag/Mix
multi sub infix:<(&)>(SetHash:D $a)   { $a.Set }
multi sub infix:<(&)>(BagHash:D $a)   { $a.Bag }
multi sub infix:<(&)>(MixHash:D $a)   { $a.Mix }
multi sub infix:<(&)>(Any $a)         { $a.Set } # also for Iterable/Map

multi sub infix:<(&)>(Setty:D $a, Setty:D $b) {
    nqp::if(
      (my $araw := $a.raw_hash) && nqp::elems($araw)
        && (my $braw := $b.raw_hash) && nqp::elems($braw),
      nqp::stmts(                              # both have elems
        nqp::if(
          nqp::islt_i(nqp::elems($araw),nqp::elems($braw)),
          nqp::stmts(                          # $a smallest, iterate over it
            (my $iter := nqp::iterator($araw)),
            (my $base := $braw)
          ),
          nqp::stmts(                          # $b smallest, iterate over that
            ($iter := nqp::iterator($braw)),
            ($base := $araw)
          )
        ),
        (my $elems := nqp::create(Rakudo::Internals::IterationSet)),
        nqp::while(
          $iter,
          nqp::if(                             # bind if in both
            nqp::existskey($base,nqp::iterkey_s(nqp::shift($iter))),
            nqp::bindkey($elems,nqp::iterkey_s($iter),nqp::iterval($iter))
          )
        ),
        nqp::create(Set).SET-SELF($elems)
      ),
      set()                                    # one/neither has elems
    )
}
multi sub infix:<(&)>(Setty:D $a, Baggy:D $b) {
    Rakudo::QuantHash.INTERSECT-BAGGIES($a.Bag, $b, bag())
}
multi sub infix:<(&)>(Baggy:D $a, Setty:D $b) {
    Rakudo::QuantHash.INTERSECT-BAGGIES($a, $b.Bag, bag())
}
multi sub infix:<(&)>(Setty:D $a, Mixy:D $b) {
    Rakudo::QuantHash.INTERSECT-BAGGIES($a.Mix, $b, mix())
}
multi sub infix:<(&)>(Mixy:D $a, Setty:D $b) {
    Rakudo::QuantHash.INTERSECT-BAGGIES($a, $b.Mix, mix())
}
multi sub infix:<(&)>(Baggy:D $a, Baggy:D $b) {
    Rakudo::QuantHash.INTERSECT-BAGGIES($a, $b, bag())
}
multi sub infix:<(&)>(Mixy:D $a, Baggy:D $b) {
    Rakudo::QuantHash.INTERSECT-BAGGIES($a, $b, mix())
}
multi sub infix:<(&)>(Baggy:D $a, Mixy:D $b) {
    Rakudo::QuantHash.INTERSECT-BAGGIES($a, $b, mix())
}
multi sub infix:<(&)>(Mixy:D $a, Mixy:D $b) {
    Rakudo::QuantHash.INTERSECT-BAGGIES($a, $b, mix())
}
multi sub infix:<(&)>(Baggy:D $a, Any:D $b) {
    nqp::if(
      nqp::istype((my $bbag := $b.Bag),Bag),
      Rakudo::QuantHash.INTERSECT-BAGGIES($a, $bbag, bag()),
      $bbag.throw
    )
}
multi sub infix:<(&)>(Any:D $a, Baggy:D $b) {
    infix:<(&)>($b, $a)
}
multi sub infix:<(&)>(Mixy:D $a, Any:D $b) {
    nqp::if(
      nqp::istype((my $bmix := $b.Mix),Mix),
      Rakudo::QuantHash.INTERSECT-BAGGIES($a, $bmix, mix()),
      $bmix.throw
    )
}
multi sub infix:<(&)>(Any:D $a, Mixy:D $b) {
    infix:<(&)>($b, $a)
}

multi sub infix:<(&)>(Map:D $a, Map:D $b) {
    nqp::if(
      nqp::eqaddr($a.keyof,Str(Any)) && nqp::eqaddr($b.keyof,Str(Any)),
      nqp::if(                               # both ordinary Str hashes
        (my $araw := nqp::getattr(nqp::decont($a),Map,'$!storage'))
          && nqp::elems($araw)
          && (my $braw := nqp::getattr(nqp::decont($b),Map,'$!storage'))
          && nqp::elems($braw),
        nqp::stmts(                          # both are initialized
          nqp::if(
            nqp::islt_i(nqp::elems($araw),nqp::elems($braw)),
            nqp::stmts(                      # $a smallest, iterate over it
              (my $iter := nqp::iterator($araw)),
              (my $base := $braw)
            ),
            nqp::stmts(                      # $b smallest, iterate over that
              ($iter := nqp::iterator($braw)),
              ($base := $araw)
            )
          ),
          (my $elems := nqp::create(Rakudo::Internals::IterationSet)),
          nqp::while(
            $iter,
            nqp::if(                         # create if in both
              nqp::existskey(
                $base,
                nqp::iterkey_s(nqp::shift($iter))
              ),
              nqp::bindkey(
                $elems,nqp::iterkey_s($iter).WHICH,nqp::iterkey_s($iter))
            )
          ),
          nqp::create(Set).SET-SELF($elems)
        ),
        set()                                # one/neither has elems
      ),
      infix:<(&)>($a.Set, $b.Set)            # object hash(es), coerce!
    )
}

multi sub infix:<(&)>(Any:D $a, Any:D $b) {
    nqp::if(
      nqp::istype((my $aset := $a.Set),Set),
      nqp::if(
        nqp::istype((my $bset := $b.Set),Set),
        infix:<(&)>($aset, $bset),
        $bset.throw
      ),
      $aset.throw
    )
}
multi sub infix:<(&)>(**@p) {
    if Rakudo::Internals.ANY_DEFINED_TYPE(@p, Mixy) {
        my $mixhash = nqp::istype(@p[0], MixHash)
            ?? MixHash.new-from-pairs(@p.shift.pairs)
            !! @p.shift.MixHash;
        for @p.map(*.Mix(:view)) -> $mix {
            $mix{$_}
              ?? ($mixhash{$_} min= $mix{$_})
              !! $mixhash.DELETE-KEY($_)
              for $mixhash.keys;
        }
        $mixhash.Mix(:view);
    }
    elsif Rakudo::Internals.ANY_DEFINED_TYPE(@p,Baggy) {
        my $baghash = nqp::istype(@p[0], BagHash)
            ?? BagHash.new-from-pairs(@p.shift.pairs)
            !! @p.shift.BagHash;
        for @p.map(*.Bag(:view)) -> $bag {
            $bag{$_}
              ?? ($baghash{$_} min= $bag{$_})
              !! $baghash.DELETE-KEY($_)
              for $baghash.keys;
        }
        $baghash.Bag(:view);
    }
    else {
        my $sethash = nqp::istype(@p[0], SetHash)
          ?? SetHash.new(@p.shift.keys)
          !! @p.shift.SetHash;
        for @p.map(*.Set(:view)) -> $set {
            $set{$_} || $sethash.DELETE-KEY($_) for $sethash.keys;
        }
        $sethash.Set(:view);
    }
}
# U+2229 INTERSECTION
only sub infix:<∩>(|p) is pure {
    infix:<(&)>(|p);
}

proto sub infix:<(-)>(|) is pure { * }
multi sub infix:<(-)>()               { set()  }
multi sub infix:<(-)>(QuantHash:D $a) { $a     } # Set/Bag/Mix
multi sub infix:<(-)>(SetHash:D $a)   { $a.Set }
multi sub infix:<(-)>(BagHash:D $a)   { $a.Bag }
multi sub infix:<(-)>(MixHash:D $a)   { $a.Mix }
multi sub infix:<(-)>(Any $a)         { $a.Set } # also for Iterable/Map

multi sub infix:<(-)>(Setty:D $a, Setty:D $b) {
    nqp::if(
      (my $araw := $a.raw_hash) && nqp::elems($araw),
      nqp::if(                                 # elems in $a
        (my $braw := $b.raw_hash) && nqp::elems($braw),
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
      (my $araw := $a.raw_hash) && nqp::elems($araw),
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
        (my $raw := $a.raw_hash) && nqp::elems($raw),
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

proto sub infix:<(^)>(|) is pure { * }
multi sub infix:<(^)>()               { set()  }
multi sub infix:<(^)>(QuantHash:D $a) { $a     } # Set/Bag/Mix
multi sub infix:<(^)>(SetHash:D $a)   { $a.Set }
multi sub infix:<(^)>(BagHash:D $a)   { $a.Bag }
multi sub infix:<(^)>(MixHash:D $a)   { $a.Mix }
multi sub infix:<(^)>(Any $a)         { $a.Set } # also for Iterable/Map

multi sub infix:<(^)>(Setty:D $a, Setty:D $b) {
    nqp::if(
      (my $araw := $a.raw_hash) && nqp::elems($araw),
      nqp::if(
        (my $braw := $b.raw_hash) && nqp::elems($braw),
        nqp::stmts(                            # both are initialized
          nqp::if(
            nqp::islt_i(nqp::elems($araw),nqp::elems($braw)),
            nqp::stmts(                        # $a smallest, iterate over it
              (my $iter  := nqp::iterator($araw)),
              (my $elems := nqp::clone($braw))
            ),
            nqp::stmts(                        # $b smallest, iterate over that
              ($iter  := nqp::iterator($braw)),
              ($elems := nqp::clone($araw))
            )
          ),
          nqp::while(
            $iter,
            nqp::if(                           # remove if in both
              nqp::existskey($elems,nqp::iterkey_s(nqp::shift($iter))),
              nqp::deletekey($elems,nqp::iterkey_s($iter)),
              nqp::bindkey($elems,nqp::iterkey_s($iter),nqp::iterval($iter))
            )
          ),
          nqp::create(Set).SET-SELF($elems)
        ),
        nqp::if(nqp::istype($a,Set),$a,$a.Set) # $b empty, so $a
      ),
      nqp::if(nqp::istype($b,Set),$b,$b.Set)   # $a empty, so $b
    )
}

multi sub infix:<(^)>(Mixy:D $a, Mixy:D $b) {
    nqp::if(
      (my $araw := $a.raw_hash) && nqp::elems($araw),
      nqp::if(
        (my $braw := $b.raw_hash) && nqp::elems($braw),
        nqp::stmts(                            # both are initialized
          nqp::if(
            nqp::islt_i(nqp::elems($araw),nqp::elems($braw)),
            nqp::stmts(                        # $a smallest, iterate over it
              (my $iter  := nqp::iterator(my $base := $araw)),
              (my $elems := nqp::clone($braw))
            ),
            nqp::stmts(                        # $b smallest, iterate over that
              ($iter  := nqp::iterator($base := $braw)),
              ($elems := nqp::clone($araw))
            )
          ),
          nqp::while(
            $iter,
            nqp::if(                           # remove if in both
              nqp::existskey($elems,nqp::iterkey_s(nqp::shift($iter))),
              nqp::if(
                (my $diff := nqp::getattr(nqp::iterval($iter),Pair,'$!value')
                  - nqp::getattr(
                      nqp::atkey($elems,nqp::iterkey_s($iter)),
                      Pair,
                      '$!value'
                    )
                ),
                nqp::bindkey(
                  $elems,
                  nqp::iterkey_s($iter),
                  nqp::p6bindattrinvres(
                    nqp::clone(nqp::iterval($iter)),Pair,'$!value',abs($diff)
                  )
                ),
                nqp::deletekey($elems,nqp::iterkey_s($iter))
              ),
              nqp::bindkey(
                $elems,
                nqp::iterkey_s($iter),
                nqp::clone(nqp::iterval($iter))
              )
            )
          ),
          nqp::create(Mix).SET-SELF($elems)
        ),
        nqp::if(nqp::istype($a,Mix),$a,$a.Mix) # $b empty, so $a
      ),
      nqp::if(nqp::istype($b,Mix),$b,$b.Mix)   # $a empty, so $b
    )
}

multi sub infix:<(^)>(Mixy:D $a, Baggy:D $b) { infix:<(^)>($a, $b.Mix) }
multi sub infix:<(^)>(Baggy:D $a, Mixy:D $b) { infix:<(^)>($a.Mix, $b) }
multi sub infix:<(^)>(Baggy:D $a, Baggy:D $b) {
    nqp::if(
      (my $araw := $a.raw_hash) && nqp::elems($araw),
      nqp::if(
        (my $braw := $b.raw_hash) && nqp::elems($braw),
        nqp::stmts(                            # both are initialized
          nqp::if(
            nqp::islt_i(nqp::elems($araw),nqp::elems($braw)),
            nqp::stmts(                        # $a smallest, iterate over it
              (my $iter  := nqp::iterator(my $base := $araw)),
              (my $elems := nqp::clone($braw))
            ),
            nqp::stmts(                        # $b smallest, iterate over that
              ($iter  := nqp::iterator($base := $braw)),
              ($elems := nqp::clone($araw))
            )
          ),
          nqp::while(
            $iter,
            nqp::if(                           # remove if in both
              nqp::existskey($elems,nqp::iterkey_s(nqp::shift($iter))),
              nqp::if(
                (my int $diff = nqp::sub_i(
                  nqp::getattr(nqp::iterval($iter),Pair,'$!value'),
                  nqp::getattr(
                    nqp::atkey($elems,nqp::iterkey_s($iter)),
                    Pair,
                    '$!value'
                  )
                )),
                nqp::bindkey(
                  $elems,
                  nqp::iterkey_s($iter),
                  nqp::p6bindattrinvres(
                    nqp::clone(nqp::iterval($iter)),
                    Pair,
                    '$!value',
                    nqp::abs_i($diff)
                  )
                ),
                nqp::deletekey($elems,nqp::iterkey_s($iter))
              ),
              nqp::bindkey($elems,nqp::iterkey_s($iter),nqp::iterval($iter))
            )
          ),
          nqp::create(Bag).SET-SELF($elems)
        ),
        nqp::if(nqp::istype($a,Bag),$a,$a.Bag) # $b empty, so $a
      ),
      nqp::if(nqp::istype($b,Bag),$b,$b.Bag)   # $a empty, so $b
    )
}

multi sub infix:<(^)>(Map:D $a, Map:D $b) {
    nqp::if(
      nqp::eqaddr($a.keyof,Str(Any)) && nqp::eqaddr($b.keyof,Str(Any)),
      nqp::if(                                    # both ordinary Str hashes
        (my $araw := nqp::getattr(nqp::decont($a),Map,'$!storage'))
          && nqp::elems($araw),
        nqp::if(                                  # $a has elems
          (my $braw := nqp::getattr(nqp::decont($b),Map,'$!storage'))
            && nqp::elems($braw),
          nqp::stmts(                             # $b also, need to check both
            (my $elems := nqp::create(Rakudo::Internals::IterationSet)),
            (my $iter := nqp::iterator($araw)),
            nqp::while(                           # check $a's keys in $b
              $iter,
              nqp::unless(
                nqp::existskey($braw,nqp::iterkey_s(nqp::shift($iter))),
                nqp::bindkey(
                  $elems,nqp::iterkey_s($iter).WHICH,nqp::iterkey_s($iter)
                )
              )
            ),
            ($iter := nqp::iterator($braw)),
            nqp::while(                           # check $b's keys in $a
              $iter,
              nqp::unless(
                nqp::existskey($araw,nqp::iterkey_s(nqp::shift($iter))),
                nqp::bindkey(
                  $elems,nqp::iterkey_s($iter).WHICH,nqp::iterkey_s($iter)
                )
              )
            ),
            nqp::create(Set).SET-SELF($elems)
          ),
          $a.Set                                  # no $b, so $a
        ),
        $b.Set                                    # no $a, so $b
      ),
      $a.Set (^) $b.Set                           # object hash(es), coerce!
    )
}

multi sub infix:<(^)>(Iterable:D $a, Iterable:D $b) {
    nqp::if(
      (my $aiterator := $a.flat.iterator).is-lazy
        || (my $biterator := $b.flat.iterator).is-lazy,
      Failure.new(X::Cannot::Lazy.new(:action('symmetric diff'),:what<set>)),
      nqp::stmts(
        (my $elems := Rakudo::QuantHash.ADD-PAIRS-TO-SET(
          nqp::create(Rakudo::Internals::IterationSet),
          $aiterator
        )),
        nqp::until(
          nqp::eqaddr((my $pulled := $biterator.pull-one),IterationEnd),
          nqp::if(
            nqp::existskey($elems,(my $WHICH := $pulled.WHICH)),
            nqp::deletekey($elems,$WHICH),
            nqp::bindkey($elems,$WHICH,$pulled)
          )
        ),
        nqp::create(Set).SET-SELF($elems)
      )
    )
}

multi sub infix:<(^)>(**@p) is pure {
    my $chain = @p.elems;

    if $chain == 1 {
        return @p[0];
    } elsif $chain == 2 {
        my ($a, $b) = @p;
        my $mixy-or-baggy = False;
        if nqp::istype($a, Mixy) || nqp::istype($b, Mixy) {
            ($a, $b) = $a.MixHash, $b.MixHash;
            $mixy-or-baggy = True;
        } elsif nqp::istype($a, Baggy) || nqp::istype($b, Baggy) {
            ($a, $b) = $a.BagHash, $b.BagHash;
            $mixy-or-baggy = True;
        }
        return  $mixy-or-baggy
                    # the set formula is not symmetric for bag/mix. this is.
                    ?? ($a (-) $b) (+) ($b (-) $a)
                    # set formula for the two-arg set.
                    !! ($a (|) $b) (-) ($b (&) $a);
    } else {
        if Rakudo::Internals.ANY_DEFINED_TYPE(@p,Baggy) {
            my $head;
            while (@p) {
                my ($a, $b);
                if $head.defined {
                    ($a, $b) = $head, @p.shift;
                } else {
                    ($a, $b) = @p.shift, @p.shift;
                }
                if nqp::istype($a, Mixy) || nqp::istype($b, Mixy) {
                    ($a, $b) = $a.MixHash, $b.MixHash;
                } elsif nqp::istype($a, Baggy) || nqp::istype($b, Baggy) {
                    ($a, $b) = $a.BagHash, $b.BagHash;
                }
                $head = ($a (-) $b) (+) ($b (-) $a);
            }
            return $head;
        } else {
            return ([(+)] @p>>.Bag).grep(*.value == 1).Set;
        }
    }
}
# U+2296 CIRCLED MINUS
only sub infix:<⊖>($a, $b) is pure {
    $a (^) $b;
}

multi sub infix:<eqv>(Setty:D \a, Setty:D \b) {
    nqp::p6bool(
      nqp::unless(
        nqp::eqaddr(a,b),
        nqp::eqaddr(a.WHAT,b.WHAT) && a.hll_hash eqv b.hll_hash
      )
    )
}

proto sub infix:<<(<=)>>($, $ --> Bool:D) is pure {*}
multi sub infix:<<(<=)>>(Setty:D $a, Setty:D $b --> Bool:D) {
    Rakudo::QuantHash.SET-IS-SUBSET($a,$b)
}
multi sub infix:<<(<=)>>(Setty:D $a, QuantHash:D $b --> Bool:D) {
    Rakudo::QuantHash.SET-IS-SUBSET($a,$b)
}
multi sub infix:<<(<=)>>(QuantHash:D $a, Setty:D $b --> Bool:D) {
    Rakudo::QuantHash.SET-IS-SUBSET($a,$b)
}
multi sub infix:<<(<=)>>(Mixy:D $a, Mixy:D $b --> Bool:D) {
    Rakudo::QuantHash.MIX-IS-SUBSET($a,$b)
}
multi sub infix:<<(<=)>>(Mixy:D $a, Baggy:D $b --> Bool:D) {
    Rakudo::QuantHash.MIX-IS-SUBSET($a,$b)
}
multi sub infix:<<(<=)>>(Baggy:D $a, Mixy:D $b --> Bool:D) {
    Rakudo::QuantHash.MIX-IS-SUBSET($a,$b)
}
multi sub infix:<<(<=)>>(Baggy:D $a, Baggy:D $b --> Bool:D) {
    nqp::stmts(
      nqp::unless(
        nqp::eqaddr(nqp::decont($a),nqp::decont($b)),
        nqp::if(
          (my $araw := $a.raw_hash)
            && nqp::elems($araw),
          nqp::if(                # number of elems in B *always* >= A
            (my $braw := $b.raw_hash)
              && nqp::isle_i(nqp::elems($araw),nqp::elems($braw))
              && (my $iter := nqp::iterator($araw)),
            nqp::while(           # number of elems in B >= A
              $iter,
              nqp::unless(
                nqp::getattr(nqp::iterval(nqp::shift($iter)),Pair,'$!value')
                  <=              # value in A should be less or equal than B
                nqp::getattr(
                  nqp::ifnull(
                    nqp::atkey($braw,nqp::iterkey_s($iter)),
                    BEGIN       # provide virtual value 0
                      nqp::p6bindattrinvres(nqp::create(Pair),Pair,'$!value',0)
                  ),
                  Pair,
                  '$!value'
                ),
                return False
              )
            ),
            return False          # number of elems in B smaller than A
          )
        )
      ),
      True
    )
}
multi sub infix:<<(<=)>>(Map:D $a, Map:D $b --> Bool:D) {
    nqp::if(
      nqp::eqaddr(nqp::decont($a),nqp::decont($b)),
      True,                       # B is alias of A
      nqp::if(                    # A and B are different
        (my $araw := nqp::getattr(nqp::decont($a),Map,'$!storage'))
          && nqp::elems($araw),
        nqp::if(                  # something in A
          nqp::eqaddr($a.keyof,Str(Any)) && nqp::eqaddr($b.keyof,Str(Any)),
          nqp::if(                # both are normal Maps
            (my $iter := nqp::iterator($araw))
              && (my $braw := nqp::getattr(nqp::decont($b),Map,'$!storage'))
              && nqp::elems($braw),
            nqp::stmts(           # something to check for in B
              nqp::while(
                $iter,
                nqp::if(
                  nqp::iterval(nqp::shift($iter)),
                  nqp::unless(    # valid in A
                    nqp::atkey($braw,nqp::iterkey_s($iter)),
                    return False  # valid elem in A isn't valid elem in B
                  )
                )
              ),
              True                # all valids in A occur as valids in B
            ),
            nqp::stmts(           # nothing to check for in B
              nqp::while(
                $iter,
                nqp::if(
                  nqp::iterval(nqp::shift($iter)),
                  return False    # valid in elem in A (and none in B)
                )
              ),
              True                # no valid elems in A
            )
          ),
          $a.Set (<=) $b.Set      # either is objectHash, so coerce
        ),
        True                      # nothing in A
      )
    )
}
multi sub infix:<<(<=)>>(Any $a, Any $b --> Bool:D) {
    nqp::if(
      nqp::eqaddr(nqp::decont($a),nqp::decont($b)),
      True,                     # X (<=) X is always True
      nqp::if(
        nqp::istype((my $aset := $a.Set(:view)),Set),
        nqp::if(
          nqp::istype((my $bset := $b.Set(:view)),Set),
          $aset (<=) $bset,
          $bset.throw
        ),
        $aset.throw
      )
    )
}
# U+2286 SUBSET OF OR EQUAL TO
only sub infix:<⊆>($a, $b --> Bool:D) is pure {
    $a (<=) $b;
}
# U+2288 NEITHER A SUBSET OF NOR EQUAL TO
only sub infix:<⊈>($a, $b --> Bool:D) is pure {
    not $a (<=) $b;
}

proto sub infix:<<(<)>>($, $ --> Bool:D) is pure {*}
multi sub infix:<<(<)>>(Setty:D $a, Setty:D $b --> Bool:D) {
    nqp::if(
      nqp::eqaddr(nqp::decont($a),nqp::decont($b)),
      False,                    # X is never a true subset of itself
      nqp::if(
        (my $braw := $b.raw_hash) && nqp::elems($braw),
        nqp::if(
          (my $araw := $a.raw_hash) && nqp::elems($araw),
          nqp::if(
            nqp::islt_i(nqp::elems($araw),nqp::elems($braw))
              && (my $iter := nqp::iterator($araw)),
            nqp::stmts(         # A has fewer elems than B
              nqp::while(
                $iter,
                nqp::unless(
                  nqp::existskey($braw,nqp::iterkey_s(nqp::shift($iter))),
                  return False  # elem in A doesn't exist in B
                )
              ),
              True              # all elems in A exist in B
            ),
            False               # number of elems in B smaller or equal to A
          ),
          True                  # no elems in A, and elems in B
        ),
        False                   # can never have fewer elems in A than in B
      )
    )
}
multi sub infix:<<(<)>>(Mixy:D $a, Baggy:D $b --> Bool:D) {
    infix:<<(<)>>($a, $b.Mix)
}
multi sub infix:<<(<)>>(Baggy:D $a, Mixy:D $b --> Bool:D) {
    infix:<<(<)>>($a.Mix, $b)
}
multi sub infix:<<(<)>>(Mixy:D $a, Mixy:D $b --> Bool:D) {
    nqp::if(
      nqp::eqaddr(nqp::decont($a),nqp::decont($b)),
      False,                    # X is never a true subset of itself
      nqp::if(
        (my $araw := $a.raw_hash) && nqp::elems($araw),
        nqp::if(                # elems in A
          (my $braw := $b.raw_hash) && nqp::elems($braw),
          nqp::stmts(           # elems in A and B
            (my $iter := nqp::iterator($araw)),
            nqp::while(         # check all values in A with B
              $iter,
              nqp::unless(
                nqp::getattr(nqp::iterval(nqp::shift($iter)),Pair,'$!value')
                  <             # value in A should be less than (virtual) B
                nqp::getattr(
                  nqp::ifnull(
                    nqp::atkey($braw,nqp::iterkey_s($iter)),
                    BEGIN       # provide virtual value 0
                      nqp::p6bindattrinvres(nqp::create(Pair),Pair,'$!value',0)
                  ),
                  Pair,
                  '$!value'
                ),
                return False
              )
            ),

            ($iter := nqp::iterator($braw)),
            nqp::while(         # check all values in B with A
              $iter,
              nqp::unless(
                nqp::getattr(nqp::iterval(nqp::shift($iter)),Pair,'$!value')
                  >             # value in B should be more than (virtual) A
                nqp::getattr(
                  nqp::ifnull(
                    nqp::atkey($araw,nqp::iterkey_s($iter)),
                    BEGIN       # provide virtual value 0
                      nqp::p6bindattrinvres(nqp::create(Pair),Pair,'$!value',0)
                  ),
                  Pair,
                  '$!value'
                ),
                return False
              )
            ),
            True                # all checks worked out, so ok
          ),
          # nothing in B, all elems in A should be < 0
          Rakudo::QuantHash.MIX-ALL-NEGATIVE($araw)
        ),
        nqp::if(                # nothing in A
          ($braw := $b.raw_hash) && nqp::elems($braw),
          # something in B, all elems in B should be > 0
          Rakudo::QuantHash.MIX-ALL-POSITIVE($braw),
          False                 # nothing in A nor B
        )
      )
    )
}
multi sub infix:<<(<)>>(Baggy:D $a, Baggy:D $b --> Bool:D) {
    nqp::if(
      nqp::eqaddr($a,$b),
      False,                    # X is never a true subset of itself
      nqp::if(
        (my $braw := $b.raw_hash) && nqp::elems($braw),
        nqp::if(
          (my $araw := $a.raw_hash) && nqp::elems($araw),
          nqp::if(
            nqp::islt_i(nqp::elems($araw),nqp::elems($braw))
              && (my $iter := nqp::iterator($araw)),
            nqp::stmts(         # A has fewer elems than B
              nqp::while(
                $iter,
                nqp::unless(
                  nqp::getattr(nqp::iterval(nqp::shift($iter)),Pair,'$!value')
                   <
                  nqp::getattr(
                    nqp::ifnull(
                      nqp::atkey($braw,nqp::iterkey_s($iter)),
                      BEGIN nqp::p6bindattrinvres(     # virtual 0
                        nqp::create(Pair),Pair,'$!value',0)
                    ),
                    Pair,
                    '$!value'
                  ),
                  return False  # elem in A not in B or same or more in B
                )
              ),
              True              # all elems in A exist in B and are less
            ),
            False               # number of elems in B smaller or equal to A
          ),
          True                  # elems in B, no elems in A
        ),
        False                   # can never have fewer elems in A than in B
      )
    )
}
multi sub infix:<<(<)>>(Map:D $a, Map:D $b --> Bool:D) {
    nqp::if(
      nqp::eqaddr(nqp::decont($a),nqp::decont($b)),
      False,                      # X is never a true subset of itself
      nqp::if(                    # A and B are different
        (my $araw := nqp::getattr(nqp::decont($a),Map,'$!storage'))
          && nqp::elems($araw),
        nqp::if(                  # something in A
          nqp::eqaddr($a.keyof,Str(Any)) && nqp::eqaddr($b.keyof,Str(Any)),
          nqp::if(                # both are normal Maps
            (my $braw := nqp::getattr(nqp::decont($b),Map,'$!storage'))
              && nqp::elems($braw)
              && (my $iter := nqp::iterator($araw)),
            nqp::stmts(           # something to check for in B
              nqp::while(
                $iter,
                nqp::if(
                  nqp::iterval(nqp::shift($iter))
                    || nqp::isfalse(nqp::atkey($braw,nqp::iterkey_s($iter))),
                  return False    # valid elem in A or invalid elem in B
                )
              ),
              True                # no valids in A, valids in B
            ),
            False                 # something in A, nothing in B
          ),
          $a.Set (<) $b.Set       # either is objectHash, so coerce
        ),
        nqp::if(                  # nothing in A
          ($braw := nqp::getattr(nqp::decont($b),Map,'$!storage'))
            && nqp::elems($braw)
            && ($iter := nqp::iterator($braw)),
          nqp::stmts(             # something in B
            nqp::while(
              $iter,
              nqp::if(
                nqp::iterval(nqp::shift($iter)),
                return True       # found valid elem in B
              )
            ),
            False                 # no valid elem in B
          ),
          False                   # nothing in B (nor A)
        )
      )
    )
}
multi sub infix:<<(<)>>(Any $a, Any $b --> Bool:D) {
    nqp::if(
      nqp::eqaddr(nqp::decont($a),nqp::decont($b)),
      False,                    # X (<) X is always False
      nqp::if(
        nqp::istype((my $aset := $a.Set(:view)),Set),
        nqp::if(
          nqp::istype((my $bset := $b.Set(:view)),Set),
          $aset (<) $bset,
          $bset.throw
        ),
        $aset.throw
      )
    )
}
# U+2282 SUBSET OF
only sub infix:<⊂>($a, $b --> Bool:D) is pure {
    $a (<) $b;
}
# U+2284 NOT A SUBSET OF
only sub infix:<⊄>($a, $b --> Bool:D) is pure {
    not $a (<) $b;
}

only sub infix:<<(>=)>>(Any $a, Any $b --> Bool:D) {
    $b (<=) $a
}
# U+2287 SUPERSET OF OR EQUAL TO
only sub infix:<⊇>($a, $b --> Bool:D) is pure {
    $b (<=) $a
}
# U+2289 NEITHER A SUPERSET OF NOR EQUAL TO
only sub infix:<⊉>($a, $b --> Bool:D) is pure {
    not $b (<=) $a
}

only sub infix:<<(>)>>(Any $a, Any $b --> Bool:D) {
    $b (<) $a
}
# U+2283 SUPERSET OF
only sub infix:<⊃>($a, $b --> Bool:D) is pure {
    $b (<) $a
}
# U+2285 NOT A SUPERSET OF
only sub infix:<⊅>($a, $b --> Bool:D) is pure {
    not $b (<) $a
}

proto sub infix:<(.)>(|) is pure { * }
multi sub infix:<(.)>()               { bag()  }
multi sub infix:<(.)>(Bag:D $a)       { $a     }
multi sub infix:<(.)>(Mix:D $a)       { $a     }
multi sub infix:<(.)>(MixHash:D $a)   { $a.Mix }
multi sub infix:<(.)>(Any $a)         { $a.Bag }

multi sub infix:<(.)>(Setty:D $a, Setty:D $b) {
    nqp::if(
      (my $elems := $a.Bag.raw_hash) && nqp::elems($elems),
      nqp::create(Bag).SET-SELF(
        Rakudo::QuantHash.MULTIPLY-SET-TO-BAG($elems,$b.raw_hash),
      ),
      bag()
    )
}

multi sub infix:<(.)>(Mixy:D $a, Mixy:D $b) {
    nqp::if(
      (my $elems := Rakudo::QuantHash.BAGGY-CLONE-RAW($a.raw_hash))
        && nqp::elems($elems),
      nqp::stmts(
        Rakudo::QuantHash.MULTIPLY-MIX-TO-MIX($elems,$b.raw_hash),
        nqp::create(Mix).SET-SELF($elems)
      ),
      mix()
    )
}

multi sub infix:<(.)>(Mixy:D $a, Baggy:D $b) { infix:<(.)>($a, $b.Mix) }
multi sub infix:<(.)>(Baggy:D $a, Mixy:D $b) { infix:<(.)>($a.Mix, $b) }
multi sub infix:<(.)>(Baggy:D $a, Baggy:D $b) {
    nqp::if(
      (my $elems := Rakudo::QuantHash.BAGGY-CLONE-RAW($a.raw_hash))
        && nqp::elems($elems),
      nqp::create(Bag).SET-SELF(
        Rakudo::QuantHash.MULTIPLY-BAG-TO-BAG($elems,$b.raw_hash),
      ),
      bag()
    )
}
multi sub infix:<(.)>(Any:D $a, Any:D $b) { $a.Bag (.) $b.Bag }

multi sub infix:<(.)>(**@p) is pure {
    if Rakudo::Internals.ANY_DEFINED_TYPE(@p,Mixy) {
        my $mixhash = nqp::istype(@p[0], MixHash)
            ?? MixHash.new-from-pairs(@p.shift.pairs)
            !! @p.shift.MixHash;
        for @p.map(*.Mix(:view)) -> $mix {
            $mix{$_}
              ?? ($mixhash{$_} *= $mix{$_})
              !! $mixhash.DELETE-KEY($_)
              for $mixhash.keys;
        }
        $mixhash.Mix(:view);
    }
    else {  # go Baggy by default
        my $baghash = nqp::istype(@p[0], BagHash)
            ?? BagHash.new-from-pairs(@p.shift.pairs)
            !! @p.shift.BagHash;
        for @p.map(*.Bag(:view)) -> $bag {
            $bag{$_}
              ?? ($baghash{$_} *= $bag{$_})
              !! $baghash.DELETE-KEY($_)
              for $baghash.keys;
        }
        $baghash.Bag(:view);
    }
}
# U+228D MULTISET MULTIPLICATION
only sub infix:<⊍>(|p) is pure {
    infix:<(.)>(|p);
}

proto sub infix:<(+)>(|) is pure { * }
multi sub infix:<(+)>()               { bag()  }
multi sub infix:<(+)>(Bag:D $a)       { $a     }
multi sub infix:<(+)>(Mix:D $a)       { $a     }
multi sub infix:<(+)>(MixHash:D $a)   { $a.Mix }
multi sub infix:<(+)>(Any $a)         { $a.Bag }

multi sub infix:<(+)>(Setty:D $a, QuantHash:D $b) {
    nqp::if(
      (my $araw := $a.raw_hash) && nqp::elems($araw),
      nqp::if(                                         # elems on left
        (my $braw := $b.raw_hash) && nqp::elems($braw),
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
      (my $araw := $a.raw_hash) && nqp::elems($araw),
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
      (my $araw := $a.raw_hash) && nqp::elems($araw),
      nqp::if(                                         # elems on left
        (my $braw := $b.raw_hash) && nqp::elems($braw),
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
      (my $araw := $a.raw_hash) && nqp::elems($araw),
      nqp::if(                                         # elems on left
        (my $braw := $b.raw_hash) && nqp::elems($braw),
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

multi sub infix:<(+)>(**@p) is pure {
    if Rakudo::Internals.ANY_DEFINED_TYPE(@p,Mixy) {
        my $mixhash = nqp::istype(@p[0], MixHash)
            ?? MixHash.new-from-pairs(@p.shift.pairs)
            !! @p.shift.MixHash;
        for @p.map(*.Mix(:view)) -> $mix {
            $mixhash{$_} += $mix{$_} for $mix.keys;
        }
        $mixhash.Mix(:view);
    }
    else {  # go Baggy by default
        my $baghash = nqp::istype(@p[0], BagHash)
            ?? BagHash.new-from-pairs(@p.shift.pairs)
            !! @p.shift.BagHash;
        for @p.map(*.Bag(:view)) -> $bag {
            $baghash{$_} += $bag{$_} for $bag.keys;
        }
        $baghash.Bag(:view);
    }
}
# U+228E MULTISET UNION
only sub infix:<⊎>(|p) is pure {
    infix:<(+)>(|p);
}

proto sub infix:<<(<+)>>($, $ --> Bool:D) is pure {*}
multi sub infix:<<(<+)>>(Setty:D \a, QuantHash:D \b --> Bool:D) {
    nqp::if(
      (my $a := a.raw_hash),
      nqp::if(
        (my $b := b.raw_hash) && nqp::isge_i(nqp::elems($b),nqp::elems($a)),
        nqp::stmts(
          (my $iter := nqp::iterator($a)),
          nqp::while(
            $iter && nqp::existskey($b,nqp::iterkey_s(nqp::shift($iter))),
            nqp::null
          ),
          nqp::p6bool(nqp::isfalse($iter))
        ),
        False
      ),
      True
    )
}
multi sub infix:<<(<+)>>(Mixy:D \a, Baggy:D \b --> Bool:D) {
    nqp::if(
      (my $a := a.raw_hash),
      nqp::if(
        (my $b := b.raw_hash) && nqp::isge_i(nqp::elems($b),nqp::elems($a)),
        nqp::stmts(
          (my $iter := nqp::iterator($a)),
          nqp::while(
            $iter,
            nqp::if(
              nqp::not_i(nqp::existskey(
                $b,
                (my $key := nqp::iterkey_s(nqp::shift($iter)))
              )) ||
              nqp::getattr(nqp::decont(nqp::atkey($a,$key)),Pair,'$!value')
                > nqp::getattr(nqp::decont(nqp::atkey($b,$key)),Pair,'$!value'),
              (return False)
            )
          ),
          True
        ),
        False
      ),
      True
    )
}
multi sub infix:<<(<+)>>(Baggy:D \a, Baggy:D \b --> Bool:D) {
    nqp::if(
      (my $a := a.raw_hash),
      nqp::if(
        (my $b := b.raw_hash) && nqp::isge_i(nqp::elems($b),nqp::elems($a)),
        nqp::stmts(
          (my $iter := nqp::iterator($a)),
          nqp::while(
            $iter,
            nqp::if(
              nqp::not_i(nqp::existskey(
                $b,
                (my $key := nqp::iterkey_s(nqp::shift($iter)))
              )) ||
              nqp::isgt_i(
                nqp::getattr(nqp::decont(nqp::atkey($a,$key)),Pair,'$!value'),
                nqp::getattr(nqp::decont(nqp::atkey($b,$key)),Pair,'$!value')
              ),
              (return False)
            )
          ),
          True
        ),
        False
      ),
      True
    )
}
multi sub infix:<<(<+)>>(QuantHash:U $a, QuantHash:U $b --> True ) {}
multi sub infix:<<(<+)>>(QuantHash:U $a, QuantHash:D $b --> True ) {}
multi sub infix:<<(<+)>>(QuantHash:D $a, QuantHash:U $b --> Bool:D ) {
    not $a.elems
}
multi sub infix:<<(<+)>>(QuantHash:D $a, QuantHash:D $b --> Bool:D ) {
    return False if $a.AT-KEY($_) > $b.AT-KEY($_) for $a.keys;
    True
}
multi sub infix:<<(<+)>>(Any $a, Any $b --> Bool:D) {
    if nqp::istype($a, Mixy) or nqp::istype($b, Mixy) {
        $a.Mix(:view) (<+) $b.Mix(:view);
    } else {
        $a.Bag(:view) (<+) $b.Bag(:view);
    }
}
# U+227C PRECEDES OR EQUAL TO
only sub infix:<≼>($a, $b --> Bool:D) is pure {
    $a (<+) $b;
}

# $a (>+) $b === $a R(<+) $b
only sub infix:<<(>+)>>($a, $b --> Bool:D) is pure {
    $b (<+) $a
}
# U+227D SUCCEEDS OR EQUAL TO
only sub infix:<≽>($a, $b --> Bool:D) is pure {
    $b (<+) $a;
}

proto sub set(|) { * }
multi sub set() { BEGIN nqp::create(Set) }
multi sub set(*@a --> Set:D) { Set.new(@a) }

proto sub bag(|) { * }
multi sub bag() { BEGIN nqp::create(Bag) }
multi sub bag(*@a --> Bag:D) { Bag.new(@a) }

proto sub mix(|) { * }
multi sub mix() { BEGIN nqp::create(Mix) }
multi sub mix(*@a --> Mix:D) { Mix.new(@a) }

# vim: ft=perl6 expandtab sw=4
