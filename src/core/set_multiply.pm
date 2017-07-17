# This file implements the following set operators:
#   (.)  set multiplication (Texas)
#   ⊍    set multiplication

proto sub infix:<(.)>(|) is pure { * }
multi sub infix:<(.)>()               { bag()  }
multi sub infix:<(.)>(Bag:D $a)       { $a     }
multi sub infix:<(.)>(Mix:D $a)       { $a     }
multi sub infix:<(.)>(MixHash:D $a)   { $a.Mix }
multi sub infix:<(.)>(Any $a)         { $a.Bag }

multi sub infix:<(.)>(Setty:D $a, Setty:D $b) {
    nqp::if(
      (my $elems := $a.Bag.RAW-HASH) && nqp::elems($elems),
      nqp::create(Bag).SET-SELF(
        Rakudo::QuantHash.MULTIPLY-SET-TO-BAG($elems,$b.RAW-HASH),
      ),
      bag()
    )
}

multi sub infix:<(.)>(Mixy:D $a, Mixy:D $b) {
    nqp::if(
      (my $elems := Rakudo::QuantHash.BAGGY-CLONE-RAW($a.RAW-HASH))
        && nqp::elems($elems),
      nqp::stmts(
        Rakudo::QuantHash.MULTIPLY-MIX-TO-MIX($elems,$b.RAW-HASH),
        nqp::create(Mix).SET-SELF($elems)
      ),
      mix()
    )
}

multi sub infix:<(.)>(Mixy:D $a, Baggy:D $b) { infix:<(.)>($a, $b.Mix) }
multi sub infix:<(.)>(Mixy:D $a, Any:D $b)   { infix:<(.)>($a, $b.Mix) }
multi sub infix:<(.)>(Baggy:D $a, Mixy:D $b) { infix:<(.)>($a.Mix, $b) }
multi sub infix:<(.)>(Any:D $a, Mixy:D $b) { infix:<(.)>($a.Mix, $b) }
multi sub infix:<(.)>(Baggy:D $a, Baggy:D $b) {
    nqp::if(
      (my $elems := Rakudo::QuantHash.BAGGY-CLONE-RAW($a.RAW-HASH))
        && nqp::elems($elems),
      nqp::create(Bag).SET-SELF(
        Rakudo::QuantHash.MULTIPLY-BAG-TO-BAG($elems,$b.RAW-HASH),
      ),
      bag()
    )
}
multi sub infix:<(.)>(Any:D $a, Any:D $b) { $a.Bag (.) $b.Bag }

multi sub infix:<(.)>(**@p) {
    my $result = @p.shift;
    $result = $result (.) @p.shift while @p;
    $result
}

# U+228D MULTISET MULTIPLICATION
my constant &infix:<⊍> := &infix:<(.)>;

# vim: ft=perl6 expandtab sw=4
