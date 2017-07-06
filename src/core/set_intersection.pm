# This file implements the following set operators:
#   (&)     intersection (Texas)
#   ∩       intersection

proto sub infix:<(&)>(|) is pure { * }
multi sub infix:<(&)>()               { set()  }
multi sub infix:<(&)>(QuantHash:D $a) { $a     } # Set/Bag/Mix
multi sub infix:<(&)>(SetHash:D $a)   { $a.Set }
multi sub infix:<(&)>(BagHash:D $a)   { $a.Bag }
multi sub infix:<(&)>(MixHash:D $a)   { $a.Mix }
multi sub infix:<(&)>(Any $a)         { $a.Set } # also for Iterable/Map

multi sub infix:<(&)>(Setty:D $a, Setty:D $b) {
    nqp::if(
      (my $araw := $a.RAW-HASH) && nqp::elems($araw)
        && (my $braw := $b.RAW-HASH) && nqp::elems($braw),
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

# vim: ft=perl6 expandtab sw=4
