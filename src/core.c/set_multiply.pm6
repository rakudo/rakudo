# This file implements the following set operators:
#   (.)  set multiplication (ASCII)
#   ⊍    set multiplication

proto sub infix:<(.)>(|) is pure {*}
multi sub infix:<(.)>()               { bag()   }
multi sub infix:<(.)>(Setty:D \a)     { a.Baggy }
multi sub infix:<(.)>(Baggy:D \a)     { a       }  # also Mixy

multi sub infix:<(.)>(Setty:D \a, Setty:D \b) {
    (my $elems := a.Bag.RAW-HASH) && nqp::elems($elems)
      ?? nqp::create(a.WHAT.Baggy).SET-SELF(
           Rakudo::QuantHash.MULTIPLY-SET-TO-BAG($elems,b.RAW-HASH),
         )
      !! a.Baggy
}

multi sub infix:<(.)>(Mixy:D \a, Mixy:D \b) {
    nqp::if(
      (my $elems := Rakudo::QuantHash.BAGGY-CLONE-RAW(a.RAW-HASH))
        && nqp::elems($elems),
      nqp::stmts(
        Rakudo::QuantHash.MULTIPLY-MIX-TO-MIX($elems,b.RAW-HASH),
        nqp::create(a.WHAT).SET-SELF($elems)
      ),
      a
    )
}

multi sub infix:<(.)>(Mixy:D  \a, Baggy:D \b) { infix:<(.)>(a, b.Mix) }
multi sub infix:<(.)>(Mixy:D  \a, Any     \b) { infix:<(.)>(a, b.Mix) }
multi sub infix:<(.)>(Setty:D \a, Mixy:D  \b) { infix:<(.)>(a.Mixy, b) }
multi sub infix:<(.)>(Baggy:D \a, Mixy:D  \b) { infix:<(.)>(a.Mixy, b) }
multi sub infix:<(.)>(Any     \a, Mixy:D  \b) { infix:<(.)>(a.Mix, b) }
multi sub infix:<(.)>(Baggy:D \a, Baggy:D \b) {
    (my $elems := Rakudo::QuantHash.BAGGY-CLONE-RAW(a.RAW-HASH))
      && nqp::elems($elems)
      ?? nqp::create(a.WHAT).SET-SELF(
           Rakudo::QuantHash.MULTIPLY-BAG-TO-BAG($elems,b.RAW-HASH),
         )
      !! a
}

multi sub infix:<(.)>(Any $, Failure:D \b) { b.throw }
multi sub infix:<(.)>(Failure:D \a, Any $) { a.throw }

# Note that we cannot create a Setty|Baggy:D,Any candidate because that will
# result in an ambiguous dispatch, so we need to hack a check for Setty|Baggy
# in here.
multi sub infix:<(.)>(Any \a, Any \b) {
    infix:<(.)>(
      nqp::isconcrete(a)
        ?? nqp::istype(a,Setty)
          ?? a.Baggy
          !! nqp::istype(a,Baggy) ?? a !! a.Bag
        !! a.Bag,
      b.Bag
    )
}

multi sub infix:<(.)>(+@p) {   # also Any
    my $iterator := @p.iterator;
    nqp::if(
      nqp::eqaddr((my $result := $iterator.pull-one),IterationEnd),
      bag(),          # nothing to process
      nqp::if(
        nqp::eqaddr((my $pulled := $iterator.pull-one),IterationEnd),
        $result.Bag,  # only 1 elem to process
        nqp::stmts(
          nqp::repeat_until(
            nqp::eqaddr(($pulled := $iterator.pull-one),IterationEnd),
            ($result := $result (.) $pulled)
          ),
          $result
        )
      )
    )
}

# U+228D MULTISET MULTIPLICATION
my constant &infix:<⊍> := &infix:<(.)>;

# vim: expandtab shiftwidth=4
