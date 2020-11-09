# This file implements the following set operators:
#   (==)  set equality (ASCII)
#   ≡     is identical to
#   ≢     is not identical to

proto sub infix:<<(==)>>($, $, *% --> Bool:D) is pure {*}
multi sub infix:<<(==)>>(Setty:D \a, Setty:D \b --> Bool:D) {
    nqp::unless(
      nqp::eqaddr(nqp::decont(a),nqp::decont(b)),
      nqp::stmts(                   # A and B not same object
        (my \araw := a.RAW-HASH),
        (my \braw := b.RAW-HASH),
        nqp::if(
          araw && braw,
          nqp::if(                  # A and B both allocated
            nqp::isne_i(nqp::elems(araw),nqp::elems(braw)),
            (return False),         # not same number of elems
            nqp::stmts(             # same number of elems in A and B
              (my \iter := nqp::iterator(araw)),
              nqp::while(           # have something to iterate over
                iter,
                nqp::unless(
                  nqp::existskey(braw,nqp::iterkey_s(nqp::shift(iter))),
                  return False      # elem in A doesn't exist in B
                )
              )
            )
          ),
          nqp::if(                  # A and B not both allocated
            (araw && nqp::elems(araw)) || (braw && nqp::elems(braw)),
            return False            # allocated side contains elements
          )
        )
      )
    );

    True
}
multi sub infix:<<(==)>>(Setty:D \a, Mixy:D  \b --> Bool:D) { a.Mix (==) b }
multi sub infix:<<(==)>>(Setty:D \a, Baggy:D \b --> Bool:D) { a.Bag (==) b }
multi sub infix:<<(==)>>(Setty:D \a, Any     \b --> Bool:D) { a (==) b.Set }

multi sub infix:<<(==)>>(Mixy:D \a, Mixy:D  \b --> Bool:D) {
    Rakudo::QuantHash.MIX-IS-EQUAL(a, b)
}
multi sub infix:<<(==)>>(Mixy:D \a, Baggy:D \b --> Bool:D) {
    Rakudo::QuantHash.MIX-IS-EQUAL(a, b)
}
multi sub infix:<<(==)>>(Mixy:D \a, Setty:D \b --> Bool:D) { a (==) b.Mix }
multi sub infix:<<(==)>>(Mixy:D \a, Any     \b --> Bool:D) { a (==) b.Mix }

multi sub infix:<<(==)>>(Baggy:D \a, Mixy:D \b --> Bool:D) {
    Rakudo::QuantHash.MIX-IS-EQUAL(a, b)
}
multi sub infix:<<(==)>>(Baggy:D \a, Baggy:D \b --> Bool:D) {
    Rakudo::QuantHash.MIX-IS-EQUAL(a, b)
}
multi sub infix:<<(==)>>(Baggy:D \a, Setty:D \b --> Bool:D) { a (==) b.Bag }
multi sub infix:<<(==)>>(Baggy:D \a, Any     \b --> Bool:D) { a (==) b.Bag }

multi sub infix:<<(==)>>(Map:D \a, Map:D \b --> Bool:D) {
    nqp::unless(
      nqp::eqaddr(nqp::decont(a),nqp::decont(b)),
      nqp::if(                        # A and B are different
        nqp::isne_i(
          nqp::elems(my \araw := nqp::getattr(nqp::decont(a),Map,'$!storage')),
          nqp::elems(my \braw := nqp::getattr(nqp::decont(b),Map,'$!storage'))
        ),
        (return False),               # different number of elements
        nqp::if(                      # same size
          nqp::istype(a,Hash::Object) || nqp::istype(b,Hash::Object),
          (return a.Set (==) b.Set),  # either is objectHash, so coerce
          nqp::stmts(                 # both are normal Maps
            (my \iter := nqp::iterator(araw)),
            nqp::while(
              iter,
              nqp::unless(
                nqp::iseq_i(
                  nqp::istrue(nqp::iterval(nqp::shift(iter))),
                  nqp::istrue(nqp::atkey(braw,nqp::iterkey_s(iter)))
                ),
                (return False)        # elem in A hasn't got same validity in B
              )
            )
          )
        )
      )
    );

    True
}

multi sub infix:<<(==)>>(Iterable:D \a, Map:D \b --> Bool:D) {
    my \iterator := a.iterator;
    my \braw := nqp::getattr(nqp::decont(b),Map,'$!storage');

    return False                          # can never find all values
      if nqp::istype(iterator,PredictiveIterator)
      && iterator.count-only < nqp::elems(braw);

    my $key;
    my $seen := nqp::hash;
    nqp::if(
      nqp::istype(b,Hash::Object),
      nqp::until(                         # object hash
        nqp::eqaddr((my \object := iterator.pull-one),IterationEnd),
        nqp::if(
          nqp::istrue(
            nqp::getattr(
              nqp::ifnull(
                nqp::atkey(braw,$key := object.WHICH),
                BEGIN                     # provide virtual value 0
                  nqp::p6bindattrinvres(nqp::create(Pair),Pair,'$!value',0)
              ),
              Pair,
              '$!value'
            )
          ),
          nqp::bindkey($seen,$key,1),
          (return False)                  # not seen or not true
        )
      ),
      nqp::until(                         # normal Map
        nqp::eqaddr(($key := iterator.pull-one),IterationEnd),
        nqp::if(
          nqp::istrue(nqp::atkey(braw,$key)),
          nqp::bindkey($seen,$key,1),
          (return False)                  # not seen or not true
        )
      )
    );

    nqp::hllbool(nqp::iseq_i(nqp::elems($seen),nqp::elems(braw)))
}

multi sub infix:<<(==)>>(Any \a, Mixy:D  \b --> Bool:D) { a.Mix (==) b     }
multi sub infix:<<(==)>>(Any \a, Baggy:D \b --> Bool:D) { a.Bag (==) b     }
multi sub infix:<<(==)>>(Any \a, Setty:D \b --> Bool:D) { a.Set (==) b     }

multi sub infix:<<(==)>>(Failure:D \a, Any $) { a.throw }
multi sub infix:<<(==)>>(Any $, Failure:D \b) { b.throw }
multi sub infix:<<(==)>>(Any \a, Any \b --> Bool:D) { a.Set (==) b.Set }

# U+2261 IDENTICAL TO
my constant &infix:<≡> := &infix:<<(==)>>;

# U+2262 NOT IDENTICAL TO
proto sub infix:<≢>($, $, *%) is pure {*}
multi sub infix:<≢>(\a, \b --> Bool:D) { not a (==) b }

# vim: ft=perl6 expandtab sw=4
