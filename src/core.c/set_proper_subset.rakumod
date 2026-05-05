# This file implements the following set operators:
#   (<)   is a proper subset of (ASCII)
#   ⊂     is a proper subset of
#   ⊄     is NOT a proper subset of
#   (>)   is a proper superset of (ASCII)
#   ⊃     is a proper superset of
#   ⊅     is NOT a proper superset of

proto sub infix:<<(<)>>($, $, *% --> Bool:D) is pure {*}
multi sub infix:<<(<)>>(Setty:D $a, Setty:D $b --> Bool:D) {
    Rakudo::QuantHash.SET-IS-PROPER-SUBSET($a, $b)
}
multi sub infix:<<(<)>>(Setty:D $a, Mixy:D  $b --> Bool:D) { $a.Mix (<) $b }
multi sub infix:<<(<)>>(Setty:D $a, Baggy:D $b --> Bool:D) { $a.Bag (<) $b }
multi sub infix:<<(<)>>(Setty:D $a, Any     \b --> Bool:D) { $a (<) b.Set }

multi sub infix:<<(<)>>(Mixy:D $a, Mixy:D $b --> Bool:D) {
    Rakudo::QuantHash.MIX-IS-PROPER-SUBSET($a,$b)
}
multi sub infix:<<(<)>>(Mixy:D $a, Baggy:D $b --> Bool:D) {
    Rakudo::QuantHash.MIX-IS-PROPER-SUBSET($a,$b)
}
multi sub infix:<<(<)>>(Mixy:D $a, Any \b --> Bool:D) {
    $a (<) b.Mix
}
multi sub infix:<<(<)>>(Baggy:D $a, Mixy:D $b --> Bool:D) {
    Rakudo::QuantHash.MIX-IS-PROPER-SUBSET($a,$b)
}
multi sub infix:<<(<)>>(Baggy:D $a, Baggy:D $b --> Bool:D) {
    Rakudo::QuantHash.BAG-IS-PROPER-SUBSET($a, $b)
}
multi sub infix:<<(<)>>(Baggy:D $a, Any \b --> Bool:D) { $a (<) b.Bag }

multi sub infix:<<(<)>>(Any \a, Mixy:D  $b --> Bool:D) { a.Mix (<) $b }
multi sub infix:<<(<)>>(Any \a, Baggy:D $b --> Bool:D) { a.Bag (<) $b }

multi sub infix:<<(<)>>(Failure:D $a, Any) { $a.throw }
multi sub infix:<<(<)>>(Any, Failure:D $b) { $b.throw }
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
