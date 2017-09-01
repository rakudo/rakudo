# This file implements the following set operators:
#   (<=)  is a subset of (Texas)
#   ⊆     is a subset of
#   ⊈     is NOT a subset of
#   (>=)  is a superset of (Texas)
#   ⊇     is a superset of
#   ⊉     is NOT a superset of

proto sub infix:<<(<=)>>($, $ --> Bool:D) is pure {*}
multi sub infix:<<(<=)>>(Setty:D $a, Setty:D $b --> Bool:D) {
    nqp::stmts(
      nqp::unless(
        nqp::eqaddr(nqp::decont($a),nqp::decont($b)),
        nqp::if(
          (my $araw := $a.RAW-HASH)
            && nqp::elems($araw),
          nqp::if(                # number of elems in B *always* >= A
            (my $braw := $b.RAW-HASH)
              && nqp::isle_i(nqp::elems($araw),nqp::elems($braw))
              && (my $iter := nqp::iterator($araw)),
            nqp::while(           # number of elems in B >= A
              $iter,
              nqp::unless(
                nqp::existskey($braw,nqp::iterkey_s(nqp::shift($iter))),
                return False      # elem in A doesn't exist in B
              )
            ),
            return False          # number of elems in B smaller than A
          )
        )
      ),
      True
    )
}
multi sub infix:<<(<=)>>(Setty:D $a, Mixy:D  $b --> Bool:D) { $a.Mix (<=) $b }
multi sub infix:<<(<=)>>(Setty:D $a, Baggy:D $b --> Bool:D) { $a.Bag (<=) $b }
multi sub infix:<<(<=)>>(Setty:D $a, Any     $b --> Bool:D) { $a (<=) $b.Set }

multi sub infix:<<(<=)>>(Mixy:D $a, Mixy:D  $b --> Bool:D) {
    Rakudo::QuantHash.MIX-IS-SUBSET($a,$b)
}
multi sub infix:<<(<=)>>(Mixy:D $a, Baggy:D $b --> Bool:D) {
    Rakudo::QuantHash.MIX-IS-SUBSET($a,$b)
}
multi sub infix:<<(<=)>>(Mixy:D $a, Setty:D $b --> Bool:D) { $a (<=) $b.Mix }
multi sub infix:<<(<=)>>(Mixy:D $a, Any     $b --> Bool:D) { $a (<=) $b.Mix }

multi sub infix:<<(<=)>>(Baggy:D $a, Mixy:D $b --> Bool:D) {
    Rakudo::QuantHash.MIX-IS-SUBSET($a,$b)
}
multi sub infix:<<(<=)>>(Baggy:D $a, Baggy:D $b --> Bool:D) {
    nqp::stmts(
      nqp::unless(
        nqp::eqaddr(nqp::decont($a),nqp::decont($b)),
        nqp::if(
          (my $araw := $a.RAW-HASH)
            && nqp::elems($araw),
          nqp::if(                # number of elems in B *always* >= A
            (my $braw := $b.RAW-HASH)
              && nqp::isle_i(nqp::elems($araw),nqp::elems($braw))
              && (my $iter := nqp::iterator($araw)),
            nqp::while(           # number of elems in B >= A
              $iter,
              nqp::unless(
                nqp::getattr(nqp::iterval(nqp::shift($iter)),Pair,'$!value')
                  <=              # value in A should be less or equal than B
                nqp::getattr(
                  nqp::ifnull(
                    nqp::atkey($braw,nqp::iterkey_s($iter)),
                    BEGIN       # provide virtual value 0
                      nqp::p6bindattrinvres(nqp::create(Pair),Pair,'$!value',0)
                  ),
                  Pair,
                  '$!value'
                ),
                return False
              )
            ),
            return False          # number of elems in B smaller than A
          )
        )
      ),
      True
    )
}
multi sub infix:<<(<=)>>(Baggy:D $a, Setty:D $b --> Bool:D) { $a (<=) $b.Bag }
multi sub infix:<<(<=)>>(Baggy:D $a, Any     $b --> Bool:D) { $a (<=) $b.Bag }

multi sub infix:<<(<=)>>(Map:D $a, Map:D $b --> Bool:D) {
    nqp::if(
      nqp::eqaddr(nqp::decont($a),nqp::decont($b)),
      True,                       # B is alias of A
      nqp::if(                    # A and B are different
        (my $araw := nqp::getattr(nqp::decont($a),Map,'$!storage'))
          && nqp::elems($araw),
        nqp::if(                  # something in A
          nqp::eqaddr($a.keyof,Str(Any)) && nqp::eqaddr($b.keyof,Str(Any)),
          nqp::if(                # both are normal Maps
            (my $iter := nqp::iterator($araw))
              && (my $braw := nqp::getattr(nqp::decont($b),Map,'$!storage'))
              && nqp::elems($braw),
            nqp::stmts(           # something to check for in B
              nqp::while(
                $iter,
                nqp::if(
                  nqp::iterval(nqp::shift($iter)),
                  nqp::unless(    # valid in A
                    nqp::atkey($braw,nqp::iterkey_s($iter)),
                    return False  # valid elem in A isn't valid elem in B
                  )
                )
              ),
              True                # all valids in A occur as valids in B
            ),
            nqp::stmts(           # nothing to check for in B
              nqp::while(
                $iter,
                nqp::if(
                  nqp::iterval(nqp::shift($iter)),
                  return False    # valid in elem in A (and none in B)
                )
              ),
              True                # no valid elems in A
            )
          ),
          $a.Set (<=) $b.Set      # either is objectHash, so coerce
        ),
        True                      # nothing in A
      )
    )
}

multi sub infix:<<(<=)>>(Any $a, Mixy:D  $b --> Bool:D) { $a.Mix (<=) $b     }
multi sub infix:<<(<=)>>(Any $a, Baggy:D $b --> Bool:D) { $a.Bag (<=) $b     }
multi sub infix:<<(<=)>>(Any $a, Setty:D $b --> Bool:D) { $a.Set (<=) $b     }
multi sub infix:<<(<=)>>(Any $a, Any     $b --> Bool:D) { $a.Set (<=) $b.Set }

multi sub infix:<<(<=)>>(Failure $a, Any     $b) { $a.throw }
multi sub infix:<<(<=)>>(Any     $a, Failure $b) { $b.throw }

# U+2286 SUBSET OF OR EQUAL TO
my constant &infix:<⊆> := &infix:<<(<=)>>;

# U+2288 NEITHER A SUBSET OF NOR EQUAL TO
only sub infix:<⊈>($a, $b --> Bool:D) is pure {
    not $a (<=) $b;
}

only sub infix:<<(>=)>>(Any $a, Any $b --> Bool:D) {
    $b (<=) $a
}
# U+2287 SUPERSET OF OR EQUAL TO
only sub infix:<⊇>($a, $b --> Bool:D) is pure {
    $b (<=) $a
}
# U+2289 NEITHER A SUPERSET OF NOR EQUAL TO
only sub infix:<⊉>($a, $b --> Bool:D) is pure {
    not $b (<=) $a
}

# vim: ft=perl6 expandtab sw=4
