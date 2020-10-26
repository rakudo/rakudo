# This file implements the following set operators:
#   (<+)    precedes (ASCII)
#   ≼       precedes
#   (>+)    succeeds (ASCII)
#   ≽       succeeds

proto sub infix:<<(<+)>>($, $, *% --> Bool:D) is pure {
    die if $*FOLDING;  # not going to constant fold something that's deprecated
    Rakudo::Deprecations.DEPRECATED(
      "set operator {$*INSTEAD // "(<=)"}",
      "",
      "6.d",
      :what("Set operator {$*WHAT // "(<+)"}"),
      :up( 1 + ?$*WHAT )
    ) unless $*INTERNAL;
    {*}
}
multi sub infix:<<(<+)>>(Setty:D \a, QuantHash:D \b --> Bool:D) {
    nqp::if(
      (my \araw := a.RAW-HASH),
      nqp::if(
        (my \braw := b.RAW-HASH)
          && nqp::isge_i(nqp::elems(braw),nqp::elems(araw)),
        nqp::stmts(
          (my \iter := nqp::iterator(araw)),
          nqp::while(
            iter && nqp::existskey(braw,nqp::iterkey_s(nqp::shift(iter))),
            nqp::null
          ),
          nqp::hllbool(nqp::isfalse(iter))
        ),
        False
      ),
      True
    )
}
multi sub infix:<<(<+)>>(Mixy:D \a, Baggy:D \b --> Bool:D) {
    nqp::if(
      (my \araw := a.RAW-HASH),
      nqp::if(
        (my \braw:= b.RAW-HASH)
          && nqp::isge_i(nqp::elems(braw),nqp::elems(araw)),
        nqp::stmts(
          (my \iter := nqp::iterator(araw)),
          nqp::while(
            iter,
            nqp::if(
              nqp::not_i(nqp::existskey(
                braw,
                (my \key := nqp::iterkey_s(nqp::shift(iter)))
              )) ||
              nqp::getattr(nqp::decont(nqp::atkey(araw,key)),Pair,'$!value')
                > nqp::getattr(nqp::decont(nqp::atkey(braw,key)),Pair,'$!value'),
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
      (my \araw := a.RAW-HASH),
      nqp::if(
        (my \braw := b.RAW-HASH)
          && nqp::isge_i(nqp::elems(braw),nqp::elems(araw)),
        nqp::stmts(
          (my \iter := nqp::iterator(araw)),
          nqp::while(
            iter,
            nqp::if(
              nqp::not_i(nqp::existskey(
                braw,
                (my \key := nqp::iterkey_s(nqp::shift(iter)))
              )) ||
              nqp::isgt_i(
                nqp::getattr(nqp::decont(nqp::atkey(araw,key)),Pair,'$!value'),
                nqp::getattr(nqp::decont(nqp::atkey(braw,key)),Pair,'$!value')
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

multi sub infix:<<(<+)>>(Any $, Failure:D $b) { $b.throw }
multi sub infix:<<(<+)>>(Failure:D $a, Any $) { $a.throw }
multi sub infix:<<(<+)>>(Any $a, Any $b --> Bool:D) {
    my $*INTERNAL = 1;
    nqp::istype($a,Mixy) || nqp::istype($b,Mixy)
      ?? infix:<<(<+)>>($a.Mix, $b.Mix)
      !! infix:<<(<+)>>($a.Bag, $b.Bag)
}

# U+227C PRECEDES OR EQUAL TO
proto sub infix:<≼>($, $, *%) is pure {*}
multi sub infix:<≼>($a, $b --> Bool:D) {
    my $*WHAT    = "≼";
    my $*INSTEAD = "⊆";
    infix:<<(<+)>>($a, $b)
}

# $a (>+) $b === $a R(<+) $b
proto sub infix:<<(>+)>>($, $, *%) is pure {*}
multi sub infix:<<(>+)>>($a, $b --> Bool:D) {
    my $*WHAT    = "(>+)";
    my $*INSTEAD = "(>=)";
    infix:<<(<+)>>($b, $a)
}

# U+227D SUCCEEDS OR EQUAL TO
proto sub infix:<≽>($, $, *%) is pure {*}
multi sub infix:<≽>($a, $b --> Bool:D) {
    my $*WHAT    = "≽";
    my $*INSTEAD = "⊇";
    infix:<<(<+)>>($b, $a)
}

# vim: expandtab shiftwidth=4
