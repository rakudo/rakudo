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

# vim: ft=perl6 expandtab sw=4
