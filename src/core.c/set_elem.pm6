# This file implements the following set operators:
#   (elem)  is an element of (ASCII)
#   ∈       is an element of
#   ∉       is NOT an element of
#   (cont)  contains (ASCII)
#   ∋       contains
#   ∌       does NOT contain

proto sub infix:<(elem)>($, $, *% --> Bool:D) is pure {*}
multi sub infix:<(elem)>(Str:D \a, Map:D \b --> Bool:D) {
    nqp::hllbool(
      nqp::istrue(
        nqp::elems(my \storage := nqp::getattr(nqp::decont(b),Map,'$!storage'))
          && nqp::if(
               nqp::istype(b,Hash::Object),
                 nqp::getattr(                                # object hash
                   nqp::ifnull(
                     nqp::atkey(storage,a.WHICH),
                     BEGIN   # provide virtual value False    # did not exist
                       nqp::p6bindattrinvres(nqp::create(Pair),Pair,'$!value',False)
                   ),
                   Pair,
                  '$!value'
                 ),
                 nqp::atkey(storage,a)                        # normal hash
               )
      )
    )
}
multi sub infix:<(elem)>(Any \a, Map:D \b --> Bool:D) {
    nqp::hllbool(
      nqp::istrue(
        nqp::elems(                                       # haz a haystack
          my \storage := nqp::getattr(nqp::decont(b),Map,'$!storage')
        ) && nqp::istype(b,Hash::Object)
          && nqp::getattr(
               nqp::ifnull(
                 nqp::atkey(storage,a.WHICH),             # exists
                 BEGIN   # provide virtual value False    # did not exist
                   nqp::p6bindattrinvres(nqp::create(Pair),Pair,'$!value',False)
               ),
               Pair,
               '$!value'
             )
      )
    )
}
multi sub infix:<(elem)>(Str:D \a, array[str] \b --> Bool:D) {
    my int $i = -1;
    nqp::while(
      nqp::islt_i(($i = nqp::add_i($i,1)),nqp::elems(b))
        && nqp::isne_s(a, nqp::atpos_s(b,$i)),
      nqp::null
    );
    nqp::hllbool(nqp::islt_i($i,nqp::elems(b)))
}

multi sub infix:<(elem)>(Int:D \a, array[int] \b --> Bool:D) {
    my int $i = -1;
    nqp::while(
      nqp::islt_i(($i = nqp::add_i($i,1)),nqp::elems(b))
        && nqp::isne_i(a, nqp::atpos_i(b,$i)),
      nqp::null
    );
    nqp::hllbool(nqp::islt_i($i,nqp::elems(b)))
}
multi sub infix:<(elem)>(Int:D \a, Range:D \b --> Bool:D) {
    b.is-int ?? b.ACCEPTS(a) !! a (elem) b.iterator
}
multi sub infix:<(elem)>(Any \a, Iterable:D \b --> Bool:D) {
    a (elem) b.iterator
}
multi sub infix:<(elem)>(Any \a, Iterator:D \b --> Bool:D) {
    nqp::if(
      b.is-lazy,
      Any.fail-iterator-cannot-be-lazy('(elem)',''),
      nqp::stmts(
        (my str $needle = a.WHICH),
        nqp::until(
          nqp::eqaddr(
            (my \pulled := nqp::decont(b.pull-one)),
            IterationEnd
          ),
          nqp::if(
            nqp::iseq_s($needle,pulled.WHICH),
            return True
          )
        ),
        False
      )
    )
}
multi sub infix:<(elem)>(Any \a, QuantHash:D \b --> Bool:D) {
    nqp::hllbool(
      (my \elems := b.RAW-HASH) ?? nqp::existskey(elems,a.WHICH) !! 0
    )
}

multi sub infix:<(elem)>(Any $, Failure:D \b) { b.throw }
multi sub infix:<(elem)>(Failure:D \a, Any $) { a.throw }
multi sub infix:<(elem)>(Any \a, Any \b) { a (elem) b.Set }

# U+2208 ELEMENT OF
my constant &infix:<∈> := &infix:<(elem)>;
# U+220A SMALL ELEMENT OF
my constant &infix:<∊> := &infix:<(elem)>;

# U+2209 NOT AN ELEMENT OF
proto sub infix:<∉>($, $, *%) is pure {*}
multi sub infix:<∉>(\a, \b --> Bool:D) { not a (elem) b }

proto sub infix:<(cont)>($, $, *%) is pure {*}
multi sub infix:<(cont)>(\a, \b --> Bool:D) { b (elem) a }

# U+220B CONTAINS AS MEMBER
my constant &infix:<∋> = &infix:<(cont)>;
# U+220D SMALL CONTAINS AS MEMBER
my constant &infix:<∍> = &infix:<(cont)>;

# U+220C DOES NOT CONTAIN AS MEMBER
proto sub infix:<∌>($, $, *%) is pure {*}
multi sub infix:<∌>(\a, \b --> Bool:D) { not b (elem) a }

# vim: expandtab shiftwidth=4
