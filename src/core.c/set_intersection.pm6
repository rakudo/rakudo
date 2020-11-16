# This file implements the following set operators:
#   (&)     intersection (ASCII)
#   ∩       intersection

proto sub infix:<(&)>(|) is pure {*}
multi sub infix:<(&)>()               { set() }
multi sub infix:<(&)>(QuantHash:D \a) { a     } # Set/Bag/Mix

multi sub infix:<(&)>(Setty:D \a, Setty:D \b) {
    nqp::if(
      (my $araw := a.RAW-HASH) && nqp::elems($araw)
        && (my $braw := b.RAW-HASH) && nqp::elems($braw),
      nqp::stmts(                              # both have elems
        nqp::if(
          nqp::islt_i(nqp::elems($araw),nqp::elems($braw)),
          nqp::stmts(                          # a smallest, iterate over it
            (my $iter := nqp::iterator($araw)),
            (my $base := $braw)
          ),
          nqp::stmts(                          # b smallest, iterate over that
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
        nqp::create(a.WHAT).SET-SELF($elems)
      ),
      nqp::if(                                 # one/neither has elems
        nqp::istype(a,Set), set(), nqp::create(SetHash)
      )
    )
}
multi sub infix:<(&)>(Setty:D \a, Baggy:D \b) {
    Rakudo::QuantHash.INTERSECT-BAGGIES(a.Baggy, b, Bag)
}
multi sub infix:<(&)>(Baggy:D \a, Setty:D \b) {
    Rakudo::QuantHash.INTERSECT-BAGGIES(a, b.Bag, Bag)
}
multi sub infix:<(&)>(Setty:D \a, Mixy:D \b) {
    Rakudo::QuantHash.INTERSECT-BAGGIES(a.Mixy, b, Mix)
}
multi sub infix:<(&)>(Mixy:D \a, Setty:D \b) {
    Rakudo::QuantHash.INTERSECT-BAGGIES(a, b.Mix, Mix)
}
multi sub infix:<(&)>(Baggy:D \a, Baggy:D \b) {
    Rakudo::QuantHash.INTERSECT-BAGGIES(a, b, Bag)
}
multi sub infix:<(&)>(Mixy:D \a, Baggy:D \b) {
    Rakudo::QuantHash.INTERSECT-BAGGIES(a, b, Mix)
}
multi sub infix:<(&)>(Baggy:D \a, Mixy:D \b) {
    Rakudo::QuantHash.INTERSECT-BAGGIES(a, b, Mix)
}
multi sub infix:<(&)>(Mixy:D \a, Mixy:D \b) {
    Rakudo::QuantHash.INTERSECT-BAGGIES(a, b, Mix)
}
multi sub infix:<(&)>(Baggy:D \a, Any:D \b) {
    nqp::istype((my $bbag := b.Bag),Bag)
      ?? Rakudo::QuantHash.INTERSECT-BAGGIES(a, $bbag, Bag)
      !! $bbag.throw
}
multi sub infix:<(&)>(Any:D \a, Baggy:D \b) {
    b.Bag (&) a
}
multi sub infix:<(&)>(Mixy:D \a, Any:D \b) {
    nqp::istype((my $bmix := b.Mix),Mix)
      ?? Rakudo::QuantHash.INTERSECT-BAGGIES(a, $bmix, Mix)
      !! $bmix.throw
}
multi sub infix:<(&)>(Any:D \a, Mixy:D \b) {
    b.Mix (&) a
}

multi sub infix:<(&)>(Map:D \a, Map:D \b) {
    nqp::if(
      nqp::istype(a,Hash::Object) || nqp::istype(b,Hash::Object),
      (a.Set (&) b.Set),                     # either is object hash, coerce!
      nqp::if(                               # both ordinary Str hashes
        nqp::elems(
          my \araw := nqp::getattr(nqp::decont(a),Map,'$!storage')
        ) && nqp::elems(
          my \braw := nqp::getattr(nqp::decont(b),Map,'$!storage')
        ),
        nqp::stmts(                          # both are initialized
          nqp::if(
            nqp::islt_i(nqp::elems(araw),nqp::elems(braw)),
            nqp::stmts(                      # a smallest, iterate over it
              (my $iter := nqp::iterator(araw)),
              (my $base := braw)
            ),
            nqp::stmts(                      # b smallest, iterate over that
              ($iter := nqp::iterator(braw)),
              ($base := araw)
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
      )
    )
}

multi sub infix:<(&)>(Any $, Failure:D \b) { b.throw }
multi sub infix:<(&)>(Failure:D \a, Any $) { a.throw }

# Note that we cannot create a Setty:D,Any candidate because that will result
# in an ambiguous dispatch, so we need to hack a check for Setty in here.
multi sub infix:<(&)>(Any \a, Any \b) {
    nqp::isconcrete(a)
      ?? nqp::istype(a,Mixy)
        ?? a (&) b.Mix
        !! nqp::istype(a,Baggy)
          ?? a (&) b.Bag
          !! nqp::istype(a,Setty)
            ?? a (&) b.Set
            !! nqp::isconcrete(b)
              ?? nqp::istype(b,Mixy)
                ?? a.Mix (&) b
                !! nqp::istype(b,Baggy)
                  ?? a.Bag (&) b
                  !! a.Set (&) b.Set
              !! a (&) b.Set
      !! a.Set (&) b
}

multi sub infix:<(&)>(+@p) { # also Any
    my $result := @p.shift;
    if @p {
        $result := $result (&) @p.shift while @p;
        $result
    }
    else {
        $result.Set
    }
}

# U+2229 INTERSECTION
my constant &infix:<∩> := &infix:<(&)>;

# vim: expandtab shiftwidth=4
