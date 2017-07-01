# This file implements the following set operators:
#   (elem)  is an element of (Texas)
#   ∈       is an element of
#   ∉       is NOT an element of
#   (cont)  contains (Texas)
#   ∋       contains
#   ∌       does NOT contain

proto sub infix:<(elem)>($, $ --> Bool:D) is pure {*}
multi sub infix:<(elem)>(Str:D $a, Map:D $b --> Bool:D) {
    nqp::p6bool(
      (my $storage := nqp::getattr(nqp::decont($b),Map,'$!storage'))
        && nqp::elems($storage)
        && nqp::if(
             nqp::eqaddr($b.keyof,Str(Any)),
             nqp::atkey($storage,$a),                     # normal hash
             nqp::getattr(                                # object hash
               nqp::ifnull(
                 nqp::atkey($storage,$a.WHICH),
                 BEGIN   # provide virtual value False    # did not exist
                   nqp::p6bindattrinvres(nqp::create(Pair),Pair,'$!value',False)
               ),
               Pair,
              '$!value'
             )
           )
    )
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

# vim: ft=perl6 expandtab sw=4
