# This file implements the following set operators:
#   (<+)    precedes (Texas)
#   ≼       precedes
#   (>+)    succeeds (Texas)
#   ≽       succeeds

proto sub infix:<<(<+)>>($, $ --> Bool:D) is pure {
    DEPRECATED(
      "set operator {$*INSTEAD // "(<=)"}",
      "",
      "6.d",
      :what("Set operator {$*WHAT // "(<+)"}"),
      :up( 1 + ?$*WHAT )
    );
    {*}
}
multi sub infix:<<(<+)>>(Setty:D \a, QuantHash:D \b --> Bool:D) {
    nqp::if(
      (my $a := a.RAW-HASH),
      nqp::if(
        (my $b := b.RAW-HASH) && nqp::isge_i(nqp::elems($b),nqp::elems($a)),
        nqp::stmts(
          (my $iter := nqp::iterator($a)),
          nqp::while(
            $iter && nqp::existskey($b,nqp::iterkey_s(nqp::shift($iter))),
            nqp::null
          ),
          nqp::p6bool(nqp::isfalse($iter))
        ),
        False
      ),
      True
    )
}
multi sub infix:<<(<+)>>(Mixy:D \a, Baggy:D \b --> Bool:D) {
    nqp::if(
      (my $a := a.RAW-HASH),
      nqp::if(
        (my $b := b.RAW-HASH) && nqp::isge_i(nqp::elems($b),nqp::elems($a)),
        nqp::stmts(
          (my $iter := nqp::iterator($a)),
          nqp::while(
            $iter,
            nqp::if(
              nqp::not_i(nqp::existskey(
                $b,
                (my $key := nqp::iterkey_s(nqp::shift($iter)))
              )) ||
              nqp::getattr(nqp::decont(nqp::atkey($a,$key)),Pair,'$!value')
                > nqp::getattr(nqp::decont(nqp::atkey($b,$key)),Pair,'$!value'),
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
      (my $a := a.RAW-HASH),
      nqp::if(
        (my $b := b.RAW-HASH) && nqp::isge_i(nqp::elems($b),nqp::elems($a)),
        nqp::stmts(
          (my $iter := nqp::iterator($a)),
          nqp::while(
            $iter,
            nqp::if(
              nqp::not_i(nqp::existskey(
                $b,
                (my $key := nqp::iterkey_s(nqp::shift($iter)))
              )) ||
              nqp::isgt_i(
                nqp::getattr(nqp::decont(nqp::atkey($a,$key)),Pair,'$!value'),
                nqp::getattr(nqp::decont(nqp::atkey($b,$key)),Pair,'$!value')
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
multi sub infix:<<(<+)>>(Any $a, Any $b --> Bool:D) {
    if nqp::istype($a, Mixy) or nqp::istype($b, Mixy) {
        $a.Mix(:view) (<+) $b.Mix(:view);
    } else {
        $a.Bag(:view) (<+) $b.Bag(:view);
    }
}
# U+227C PRECEDES OR EQUAL TO
only sub infix:<≼>($a, $b --> Bool:D) is pure {
    my $*WHAT    = "≼";
    my $*INSTEAD = "⊆";
    infix:<<(<+)>>($a, $b)
}

# $a (>+) $b === $a R(<+) $b
only sub infix:<<(>+)>>($a, $b --> Bool:D) is pure {
    my $*WHAT    = "(>+)";
    my $*INSTEAD = "(>=)";
    infix:<<(<+)>>($b, $a)
}
# U+227D SUCCEEDS OR EQUAL TO
only sub infix:<≽>($a, $b --> Bool:D) is pure {
    my $*WHAT    = "≽";
    my $*INSTEAD = "⊇";
    infix:<<(<+)>>($b, $a)
}

# vim: ft=perl6 expandtab sw=4
