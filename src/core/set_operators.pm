
proto sub infix:<(elem)>($, $ --> Bool:D) is pure {*}
multi sub infix:<(elem)>(Str:D $a, Map:D $b --> Bool:D) {
    $b.AT-KEY($a).Bool;
}
multi sub infix:<(elem)>(Any $a, QuantHash:D $b --> Bool:D) {
    nqp::p6bool(
      (my $elems := nqp::getattr($b.raw_hash,Map,'$!storage'))
        && nqp::existskey($elems,$a.WHICH)
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
    $a !(elem) $b;
}

proto sub infix:<(cont)>($, $ --> Bool:D) is pure {*}
multi sub infix:<(cont)>(Map:D $a, Str:D $b --> Bool:D) {
    $a.AT-KEY($b).Bool
}
multi sub infix:<(cont)>(QuantHash:D $a, Any $b --> Bool:D) {
    nqp::p6bool(
      (my $elems := nqp::getattr($a.raw_hash,Map,'$!storage'))
        && nqp::existskey($elems,$b.WHICH)
    )
}
multi sub infix:<(cont)>(Any $a, Any $b --> Bool:D) {
    $a.Set(:view) (cont) $b;
}
# U+220B CONTAINS AS MEMBER
only sub infix:<∋>($a, $b --> Bool:D) is pure {
    $a (cont) $b;
}
# U+220C DOES NOT CONTAIN AS MEMBER
only sub infix:<∌>($a, $b --> Bool:D) is pure {
    $a !(cont) $b;
}

proto sub infix:<(|)>(|) is pure { * }
multi sub infix:<(|)>()               { set()  }
multi sub infix:<(|)>(QuantHash:D $a) { $a     } # Set/Map/Mix
multi sub infix:<(|)>(SetHash:D $a)   { $a.Set }
multi sub infix:<(|)>(BagHash:D $a)   { $a.Bag }
multi sub infix:<(|)>(MixHash:D $a)   { $a.Mix }
multi sub infix:<(|)>(Any $a)         { $a.Set } # also for Iterable/Map

multi sub infix:<(|)>(Setty:D $a, Setty:D $b) {
    nqp::if(
      (my $araw := nqp::getattr($a.raw_hash,Map,'$!storage')),
      nqp::if(                                    # first is initialized
        (my $braw := nqp::getattr($b.raw_hash,Map,'$!storage')),
        nqp::stmts(                               # second is initialized
          (my $elems := nqp::clone($araw)),
          (my $iter := nqp::iterator($braw)),
          nqp::while(                             # loop over keys of second
            $iter,
            nqp::bindkey(                         # bind into clone of first
              $elems,
              nqp::iterkey_s(my $tmp := nqp::shift($iter)),
              nqp::iterval($tmp)
            )
          ),
          nqp::create(Set).SET-SELF($elems)       # make it a Set
        ),
        $a.Set                                    # no second, so first
      ),
      nqp::if(                                    # no first
        nqp::getattr($b.raw_hash,Map,'$!storage'),
        $b.Set,                                   # but second
        set()                                     # both empty
      )
    )
}

multi sub infix:<(|)>(Mixy:D $a, Mixy:D $b) {
    nqp::if(
      (my $araw := nqp::getattr($a.raw_hash,Map,'$!storage')),
      nqp::if(                                    # first is initialized
        (my $braw := nqp::getattr($b.raw_hash,Map,'$!storage')),
        nqp::stmts(                               # second is initialized
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
        nqp::getattr($b.raw_hash,Map,'$!storage'),
        $b.Mix,                                   # but second
        mix()                                     # both empty
      )
    )
}

multi sub infix:<(|)>(Mixy:D $a, Baggy:D $b) { infix:<(|)>($a, $b.Mix) }
multi sub infix:<(|)>(Baggy:D $a, Mixy:D $b) { infix:<(|)>($a.Mix, $b) }
multi sub infix:<(|)>(Baggy:D $a, Baggy:D $b) {
    nqp::if(
      (my $araw := nqp::getattr($a.raw_hash,Map,'$!storage')),
      nqp::if(                                    # first is initialized
        (my $braw := nqp::getattr($b.raw_hash,Map,'$!storage')),
        nqp::stmts(                               # second is initialized
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
        nqp::getattr($b.raw_hash,Map,'$!storage'),
        $b.Bag,                                   # but second
        bag()                                     # both empty
      )
    )
}

multi sub infix:<(|)>(Map:D $a, Map:D $b) {
    nqp::if(
      nqp::eqaddr($a.keyof,Str(Any)) && nqp::eqaddr($b.keyof,Str(Any)),
      nqp::stmts(                                 # both ordinary Str hashes
        (my $elems := nqp::create(Rakudo::Internals::IterationSet)),
        nqp::if(
          (my $raw := nqp::getattr(nqp::decont($a),Map,'$!storage'))
            && (my $iter := nqp::iterator($raw)),
          nqp::while(
            $iter,
            nqp::if(
              nqp::iterval(my $tmp := nqp::shift($iter)),
              nqp::bindkey(
                $elems,nqp::iterkey_s($tmp).WHICH,nqp::iterkey_s($tmp))
            )
          )
        ),
        nqp::if(
          ($raw := nqp::getattr(nqp::decont($b),Map,'$!storage'))
            && ($iter := nqp::iterator($raw)),
          nqp::while(
            $iter,
            nqp::if(
              nqp::iterval($tmp := nqp::shift($iter)),
              nqp::bindkey(
                $elems,nqp::iterkey_s($tmp).WHICH,nqp::iterkey_s($tmp))
            )
          )
        ),
        nqp::if(
          nqp::elems($elems),
          nqp::create(Set).SET-SELF($elems),
          set()
        )
      ),
      $a.Set (|) $b.Set                           # object hash(es), coerce!
    )
}

multi sub infix:<(|)>(Iterable:D $a, Iterable:D $b) {
    nqp::if(
      (my $aiterator := $a.flat.iterator).is-lazy
        || (my $biterator := $b.flat.iterator).is-lazy,
      Failure.new(X::Cannot::Lazy.new(:action<union>,:what<set>)),
      nqp::if(
        nqp::elems(
          (my $elems := Set.fill_IterationSet(
            Set.fill_IterationSet(
              nqp::create(Rakudo::Internals::IterationSet),
              $aiterator
            ),
            $biterator
          ))
        ),
        nqp::create(Set).SET-SELF($elems),
        set()
      )
    )
}
multi sub infix:<(|)>(**@p) {
    return set() unless @p;

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
multi sub infix:<(&)>(QuantHash:D $a) { $a     } # Set/Map/Mix
multi sub infix:<(&)>(SetHash:D $a)   { $a.Set }
multi sub infix:<(&)>(BagHash:D $a)   { $a.Bag }
multi sub infix:<(&)>(MixHash:D $a)   { $a.Mix }
multi sub infix:<(&)>(Any $a)         { $a.Set } # also for Iterable/Map

multi sub infix:<(&)>(Setty:D $a, Setty:D $b) {
    nqp::if(
      (my $araw := nqp::getattr($a.raw_hash,Map,'$!storage'))
        && (my $braw := nqp::getattr($b.raw_hash,Map,'$!storage')),
      nqp::stmts(                              # both are initialized
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
            nqp::existskey($base,nqp::iterkey_s(my $tmp := nqp::shift($iter))),
            nqp::bindkey($elems,nqp::iterkey_s($tmp),nqp::iterval($tmp))
          )
        ),
        nqp::if(
          nqp::elems($elems),
          nqp::create(Set).SET-SELF($elems),   # overlap, so make it a Set
          set()                                # nothing to see here
        )
      ),
      set()                                    # one/neither initialized
    )
}

multi sub infix:<(&)>(Mixy:D $a, Mixy:D $b) {
    nqp::if(
      (my $araw := nqp::getattr($a.raw_hash,Map,'$!storage'))
        && (my $braw := nqp::getattr($b.raw_hash,Map,'$!storage')),
      nqp::stmts(                              # both are initialized
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
            nqp::existskey($base,nqp::iterkey_s(my $tmp := nqp::shift($iter))),
            nqp::bindkey(
              $elems,
              nqp::iterkey_s($tmp),
              nqp::if(
                nqp::getattr(
                  nqp::decont(nqp::iterval($tmp)),
                  Pair,
                  '$!value'
                ) < nqp::getattr(              # must be HLL comparison
                      nqp::atkey($base,nqp::iterkey_s($tmp)),
                      Pair,
                      '$!value'
                    ),
                nqp::iterval($tmp),
                nqp::atkey($base,nqp::iterkey_s($tmp))
              )
            )
          )
        ),
        nqp::if(
          nqp::elems($elems),
          nqp::create(Mix).SET-SELF($elems),   # overlap, so make it a Mix
          mix()                                # nothing to see here
        )
      ),
      mix()                                    # one/neither initialized
    )
}

multi sub infix:<(&)>(Mixy:D $a, Baggy:D $b) { infix:<(&)>($a, $b.Mix) }
multi sub infix:<(&)>(Baggy:D $a, Mixy:D $b) { infix:<(&)>($a.Mix, $b) }
multi sub infix:<(&)>(Baggy:D $a, Baggy:D $b) {
    nqp::if(
      (my $araw := nqp::getattr($a.raw_hash,Map,'$!storage'))
        && (my $braw := nqp::getattr($b.raw_hash,Map,'$!storage')),
      nqp::stmts(                            # both are initialized
        nqp::if(
          nqp::islt_i(nqp::elems($araw),nqp::elems($braw)),
          nqp::stmts(                        # $a smallest, iterate over it
            (my $iter := nqp::iterator($araw)),
            (my $base := $braw)
          ),
          nqp::stmts(                        # $b smallest, iterate over that
            ($iter := nqp::iterator($braw)),
            ($base := $araw)
          )
        ),
        (my $elems := nqp::create(Rakudo::Internals::IterationSet)),
        nqp::while(
          $iter,
          nqp::if(                           # bind if in both
          nqp::existskey(
              $base,
              nqp::iterkey_s(my $tmp := nqp::shift($iter))
            ),
            nqp::bindkey(
              $elems,
              nqp::iterkey_s($tmp),
              nqp::if(
                nqp::isle_i(
                  nqp::getattr(
                    nqp::decont(nqp::iterval($tmp)),
                    Pair,
                    '$!value'
                  ),
                  nqp::getattr(
                    nqp::atkey($base,nqp::iterkey_s($tmp)),
                    Pair,
                    '$!value'
                  )
                ),
                nqp::iterval($tmp),
                nqp::atkey($base,nqp::iterkey_s($tmp))
              )
            )
          )
        ),
        nqp::if(
          nqp::elems($elems),
          nqp::create(Bag).SET-SELF($elems), # overlap, so make it a Bag
          bag()                              # nothing to see here
        )
      ),
      bag()                                  # one/neither initialized
    )
}

multi sub infix:<(&)>(Map:D $a, Map:D $b) {
    nqp::if(
      nqp::eqaddr($a.keyof,Str(Any)) && nqp::eqaddr($b.keyof,Str(Any)),
      nqp::if(                               # both ordinary Str hashes
        (my $araw := nqp::getattr(nqp::decont($a),Map,'$!storage'))
          && (my $braw := nqp::getattr(nqp::decont($b),Map,'$!storage')),
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
                nqp::iterkey_s(my $tmp := nqp::shift($iter))
              ),
              nqp::bindkey(
                $elems,nqp::iterkey_s($tmp).WHICH,nqp::iterkey_s($tmp))
            )
          ),
          nqp::if(
            nqp::elems($elems),
            nqp::create(Set).SET-SELF($elems),
            set()
          )
        ),
        set()                                # one/neither initialized
      ),
      $a.Set (&) $b.Set                      # object hash(es), coerce!
    )
}

multi sub infix:<(&)>(Iterable:D $a, Iterable:D $b) {
    nqp::if(
      (my $aiterator := $a.flat.iterator).is-lazy
        || (my $biterator := $b.flat.iterator).is-lazy,
      Failure.new(X::Cannot::Lazy.new(:action<intersect>,:what<set>)),
      nqp::if(                                # won't hang
        nqp::elems(my $base := Set.fill_IterationSet(
          nqp::create(Rakudo::Internals::IterationSet),
          $aiterator
        )),
        nqp::stmts(                           # have something to look up in
          (my $elems := nqp::create(Rakudo::Internals::IterationSet)),
          nqp::until(
            nqp::eqaddr((my $pulled := $biterator.pull-one),IterationEnd),
            nqp::if(
              nqp::existskey($base,(my $which := $pulled.WHICH)),
              nqp::bindkey($elems,$which,$pulled)
            )
          ),
          nqp::if(
            nqp::elems($elems),
            nqp::create(Set).SET-SELF($elems),# found something
            set()                             # no matches
          )
        ),
        set()                                 # nothing to look up in, bye!
      )
    )
}
multi sub infix:<(&)>(**@p) {
    return set() unless @p;

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
multi sub infix:<(-)>(QuantHash:D $a) { $a     } # Set/Map/Mix
multi sub infix:<(-)>(SetHash:D $a)   { $a.Set }
multi sub infix:<(-)>(BagHash:D $a)   { $a.Bag }
multi sub infix:<(-)>(MixHash:D $a)   { $a.Mix }
multi sub infix:<(-)>(Any $a)         { $a.Set } # also for Iterable/Map

multi sub infix:<(-)>(**@p) {
    return set() unless @p;

    if Rakudo::Internals.ANY_DEFINED_TYPE(@p,Mixy) {
        my $mixhash = nqp::istype(@p[0], MixHash)
            ?? MixHash.new-from-pairs(@p.shift.pairs)
            !! @p.shift.MixHash;
        for @p.map(*.Mix(:view)) -> $mix {
            $mix{$_} < $mixhash{$_}
              ?? ($mixhash{$_} -= $mix{$_})
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

only sub infix:<(^)>(**@p) is pure {
    return set() unless my $chain = @p.elems;

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
        if Rakudo::Internals.ANY_DEFINED_TYPE(@p,Mixy)
             || Rakudo::Internals.ANY_DEFINED_TYPE(@p,Baggy) {
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
        nqp::eqaddr(a.WHAT,b.WHAT)
          && nqp::getattr(nqp::decont(a),a.WHAT,'%!elems')
               eqv nqp::getattr(nqp::decont(b),b.WHAT,'%!elems')
      )
    )
}

proto sub infix:<<(<=)>>($, $ --> Bool:D) is pure {*}
multi sub infix:<<(<=)>>(Any $a, Any $b --> Bool:D) {
    $a.Set(:view) (<=) $b.Set(:view);
}
multi sub infix:<<(<=)>>(Setty $a, Setty $b --> Bool:D) {
    $a <= $b and so $a.keys.all (elem) $b
}
# U+2286 SUBSET OF OR EQUAL TO
only sub infix:<⊆>($a, $b --> Bool:D) is pure {
    $a (<=) $b;
}
# U+2288 NEITHER A SUBSET OF NOR EQUAL TO
only sub infix:<⊈>($a, $b --> Bool:D) is pure {
    $a !(<=) $b;
}

proto sub infix:<<(<)>>($, $ --> Bool:D) is pure {*}
multi sub infix:<<(<)>>(Any $a, Any $b --> Bool:D) {
    $a.Set(:view) (<) $b.Set(:view);
}
multi sub infix:<<(<)>>(Setty $a, Setty $b --> Bool:D) {
    $a < $b and so $a.keys.all (elem) $b;
}
# U+2282 SUBSET OF
only sub infix:<⊂>($a, $b --> Bool:D) is pure {
    $a (<) $b;
}
# U+2284 NOT A SUBSET OF
only sub infix:<⊄>($a, $b --> Bool:D) is pure {
    $a !(<) $b;
}

proto sub infix:<<(>=)>>($, $ --> Bool:D) is pure {*}
multi sub infix:<<(>=)>>(Any $a, Any $b --> Bool:D) {
    $a.Set(:view) (>=) $b.Set(:view);
}
multi sub infix:<<(>=)>>(Setty $a, Setty $b --> Bool:D) {
    $a >= $b and so $b.keys.all (elem) $a;
}
# U+2287 SUPERSET OF OR EQUAL TO
only sub infix:<⊇>($a, $b --> Bool:D) is pure {
    $a (>=) $b;
}
# U+2289 NEITHER A SUPERSET OF NOR EQUAL TO
only sub infix:<⊉>($a, $b --> Bool:D) is pure {
    $a !(>=) $b;
}

proto sub infix:<<(>)>>($, $ --> Bool:D) is pure {*}
multi sub infix:<<(>)>>(Any $a, Any $b --> Bool:D) {
    $a.Set(:view) (>) $b.Set(:view);
}
multi sub infix:<<(>)>>(Setty $a, Setty $b --> Bool:D) {
    $a > $b and so $b.keys.all (elem) $a;
}
# U+2283 SUPERSET OF
only sub infix:<⊃>($a, $b --> Bool:D) is pure {
    $a (>) $b;
}
# U+2285 NOT A SUPERSET OF
only sub infix:<⊅>($a, $b --> Bool:D) is pure {
    $a !(>) $b;
}

only sub infix:<(.)>(**@p) is pure {
    return bag() unless @p;

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

only sub infix:<(+)>(**@p) is pure {
    return bag() unless @p;

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
      (my $a := nqp::getattr(a.raw_hash,Map,'$!storage')),
      nqp::if(
        (my $b := nqp::getattr(b.raw_hash,Map,'$!storage'))
          && nqp::isge_i(nqp::elems($b),nqp::elems($a)),
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
      (my $a := nqp::getattr(a.raw_hash,Map,'$!storage')),
      nqp::if(
        (my $b := nqp::getattr(b.raw_hash,Map,'$!storage'))
          && nqp::isge_i(nqp::elems($b),nqp::elems($a)),
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
      (my $a := nqp::getattr(a.raw_hash,Map,'$!storage')),
      nqp::if(
        (my $b := nqp::getattr(b.raw_hash,Map,'$!storage'))
          && nqp::isge_i(nqp::elems($b),nqp::elems($a)),
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
