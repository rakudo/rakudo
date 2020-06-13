# This file implements the following set operators:
#   (<)   is a proper subset of (ASCII)
#   ⊂     is a proper subset of
#   ⊄     is NOT a proper subset of
#   (>)   is a proper superset of (ASCII)
#   ⊃     is a proper superset of
#   ⊅     is NOT a proper superset of

proto sub infix:<<(<)>>($, $, *% --> Bool:D) is pure {*}
multi sub infix:<<(<)>>(Setty:D \a, Setty:D \b --> Bool:D) {
    nqp::if(
      nqp::eqaddr(nqp::decont(a),nqp::decont(b)),
      False,                    # X is never a true subset of itself
      nqp::if(
        (my $braw := b.RAW-HASH) && nqp::elems($braw),
        nqp::if(
          (my $araw := a.RAW-HASH) && nqp::elems($araw),
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
multi sub infix:<<(<)>>(Setty:D \a, Mixy:D  \b --> Bool:D) { a.Mix (<) b }
multi sub infix:<<(<)>>(Setty:D \a, Baggy:D \b --> Bool:D) { a.Bag (<) b }
multi sub infix:<<(<)>>(Setty:D \a, Any     \b --> Bool:D) { a (<) b.Set }

multi sub infix:<<(<)>>(Mixy:D \a, Mixy:D \b --> Bool:D) {
    Rakudo::QuantHash.MIX-IS-PROPER-SUBSET(a,b)
}
multi sub infix:<<(<)>>(Mixy:D \a, Baggy:D \b --> Bool:D) {
    Rakudo::QuantHash.MIX-IS-PROPER-SUBSET(a,b)
}
multi sub infix:<<(<)>>(Mixy:D \a, Any \b --> Bool:D) {
    a (<) b.Mix
}
multi sub infix:<<(<)>>(Baggy:D \a, Mixy:D \b --> Bool:D) {
    Rakudo::QuantHash.MIX-IS-PROPER-SUBSET(a,b)
}
multi sub infix:<<(<)>>(Baggy:D \a, Baggy:D \b --> Bool:D) {
    nqp::if(
      nqp::eqaddr(nqp::decont(a),nqp::decont(b)),
      False,                    # never proper subset of self

      nqp::if(                  # different objects
        (my \araw := a.RAW-HASH) && (my \iter := nqp::iterator(araw)),
        nqp::if(                # elements on left
          (my \braw := b.RAW-HASH) && nqp::elems(braw),
          nqp::if(              # elements on both sides
            nqp::isle_i(nqp::elems(araw),nqp::elems(braw)),
            nqp::stmts(         # equal number of elements on either side
              (my int $less = 0),
              nqp::while(
                iter,
                nqp::if(
                  (my \left := nqp::getattr(
                    nqp::iterval(nqp::shift(iter)),
                    Pair,
                    '$!value'
                  ))
                   >
                  (my \right := nqp::getattr(
                    nqp::ifnull(
                      nqp::atkey(braw,nqp::iterkey_s(iter)),
                      BEGIN nqp::p6bindattrinvres(     # virtual 0
                        nqp::create(Pair),Pair,'$!value',0)
                    ),
                    Pair,
                    '$!value'
                  )),
                  (return False), # too many on left, we're done
                  nqp::unless($less,$less = left < right)
                )
              ),
              nqp::hllbool(      # ok so far, must have lower total or fewer keys
                $less || nqp::islt_i(nqp::elems(araw),nqp::elems(braw))
              )
            ),
            False               # more keys on left
          ),
          False                 # keys on left, no keys on right
        ),
        nqp::hllbool(            # no keys on left
          (my \raw := b.RAW-HASH) ?? nqp::elems(raw) !! 0
        )
      )
    )
}
multi sub infix:<<(<)>>(Baggy:D \a, Any \b --> Bool:D) { a (<) b.Bag }

multi sub infix:<<(<)>>(Any \a, Mixy:D  \b --> Bool:D) { a.Mix (<) b }
multi sub infix:<<(<)>>(Any \a, Baggy:D \b --> Bool:D) { a.Bag (<) b }

multi sub infix:<<(<)>>(Failure:D \a, Any $) { a.throw }
multi sub infix:<<(<)>>(Any $, Failure:D \b) { b.throw }
multi sub infix:<<(<)>>(Any \a, Any \b --> Bool:D) {
    a.Set (<) b.Set
}

# U+2282 SUBSET OF
my constant &infix:<⊂> := &infix:<<(<)>>;

# U+2284 NOT A SUBSET OF
proto sub infix:<⊄>($, $, *%) is pure {*}
multi sub infix:<⊄>(\a, \b --> Bool:D) { not a (<) b }

proto sub infix:<<(>)>>($, $, *%) is pure {*}
multi sub infix:<<(>)>>(\a, \b --> Bool:D) { b (<) a }

# U+2283 SUPERSET OF
proto sub infix:<⊃>($, $, *%) is pure {*}
multi sub infix:<⊃>(\a, \b --> Bool:D) { b (<) a }

# U+2285 NOT A SUPERSET OF
proto sub infix:<⊅>($, $, *%) is pure {*}
multi sub infix:<⊅>(\a, \b --> Bool:D) { not b (<) a }

# vim: expandtab shiftwidth=4
