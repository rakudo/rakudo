
proto sub infix:<(elem)>($, $ --> Bool) is pure {*}
multi sub infix:<(elem)>($a, Any $b --> Bool) {
    $a (elem) $b.Set(:view);
}
multi sub infix:<(elem)>($a, Set $b --> Bool) {
    $b.EXISTS-KEY($a);
}
# U+2208 ELEMENT OF
only sub infix:<∈>($a, $b --> Bool) is pure {
    $a (elem) $b;
}
# U+2209 NOT AN ELEMENT OF
only sub infix:<∉>($a, $b --> Bool) is pure {
    $a !(elem) $b;
}

proto sub infix:<(cont)>($, $ --> Bool) is pure {*}
multi sub infix:<(cont)>(Any $a, $b --> Bool) {
    $a.Set(:view) (cont) $b;
}
multi sub infix:<(cont)>(Set $a, $b --> Bool) {
    $a.EXISTS-KEY($b);
}
# U+220B CONTAINS AS MEMBER
only sub infix:<∋>($a, $b --> Bool) is pure {
    $a (cont) $b;
}
# U+220C DOES NOT CONTAIN AS MEMBER
only sub infix:<∌>($a, $b --> Bool) is pure {
    $a !(cont) $b;
}

only sub infix:<(|)>(**@p) is pure {
    if @p.first(Mixy) {
        my $mixhash = nqp::istype(@p[0], MixHash)
            ?? MixHash.new-from-pairs(@p.shift.pairs)
            !! @p.shift.MixHash;
        for @p.map(*.Mix(:view)) -> $mix {
            $mixhash{$_} max= $mix{$_} for $mix.keys;
        }
        $mixhash.Mix(:view);
    } elsif @p.first(Baggy) {
        my $baghash = nqp::istype(@p[0], BagHash)
            ?? BagHash.new-from-pairs(@p.shift.pairs)
            !! @p.shift.BagHash;
        for @p.map(*.Bag(:view)) -> $bag {
            $baghash{$_} max= $bag{$_} for $bag.keys;
        }
        $baghash.Bag(:view);
    } else {
        Set.new( @p.map(*.Set(:view).keys.Slip) );
    }
}
# U+222A UNION
only sub infix:<∪>(|p) is pure {
    infix:<(|)>(|p);
}

only sub infix:<(&)>(**@p) is pure {
    return set() unless @p;

    if @p.first(Mixy) {
        my $mixhash = nqp::istype(@p[0], MixHash)
            ?? MixHash.new-from-pairs(@p.shift.pairs)
            !! @p.shift.MixHash;
        for @p.map(*.Mix(:view)) -> $mix {
            $mix{$_}
              ?? ($mixhash{$_} min= $mix{$_})
              !! $mixhash.DELETE-KEY($_)
              for $mixhash.keys;
        }
        $mixhash.Mix(:view);
    } elsif @p.first(Baggy) {
        my $baghash = nqp::istype(@p[0], BagHash)
            ?? BagHash.new-from-pairs(@p.shift.pairs)
            !! @p.shift.BagHash;
        for @p.map(*.Bag(:view)) -> $bag {
            $bag{$_}
              ?? ($baghash{$_} min= $bag{$_})
              !! $baghash.DELETE-KEY($_)
              for $baghash.keys;
        }
        $baghash.Bag(:view);
    } else {
        my $sethash = nqp::istype(@p[0], SetHash)
          ?? SetHash.new(@p.shift.keys)
          !! @p.shift.SetHash;
        for @p.map(*.Set(:view)) -> $set {
            $set{$_} || $sethash.DELETE-KEY($_) for $sethash.keys;
        }
        $sethash.Set(:view);
    }
}
# U+2229 INTERSECTION
only sub infix:<∩>(|p) is pure {
    infix:<(&)>(|p);
}

only sub infix:<(-)>(**@p) is pure {
    return set() unless @p;

    if @p.first(Mixy) {
        my $mixhash = nqp::istype(@p[0], MixHash)
            ?? MixHash.new-from-pairs(@p.shift.pairs)
            !! @p.shift.MixHash;
        for @p.map(*.Mix(:view)) -> $mix {
            $mix{$_} < $mixhash{$_}
              ?? ($mixhash{$_} -= $mix{$_})
              !! $mixhash.DELETE-KEY($_)
              for $mixhash.keys;
        }
        $mixhash.Mix(:view);
    } elsif @p.first(Baggy) {
        my $baghash = nqp::istype(@p[0], BagHash)
            ?? BagHash.new-from-pairs(@p.shift.pairs)
            !! @p.shift.BagHash;
        for @p.map(*.Bag(:view)) -> $bag {
            $bag{$_} < $baghash{$_}
              ?? ($baghash{$_} -= $bag{$_})
              !! $baghash.DELETE-KEY($_)
              for $baghash.keys;
        }
        $baghash.Bag(:view);
    } else {
        my $sethash = nqp::istype(@p[0],SetHash)
          ?? SetHash.new(@p.shift.keys)
          !! @p.shift.SetHash;
        for @p.map(*.Set(:view)) -> $set {
            $set{$_} && $sethash.DELETE-KEY($_) for $sethash.keys;
        }
        $sethash.Set(:view);
    }
}
# U+2216 SET MINUS
only sub infix:<∖>(|p) is pure {
    infix:<(-)>(|p);
}
only sub infix:<(^)>(**@p) is pure {
    Set.new(BagHash.new(@p.map(*.Set(:view).keys.Slip)).pairs.map({.key if .value == 1}));
}
# U+2296 CIRCLED MINUS
only sub infix:<⊖>($a, $b --> Setty) is pure {
    $a (^) $b;
}


only sub infix:<<(^+)>>(**@p) {
    if @p.first(Mixy) {
        my $mixhash = nqp::istype(@p[0],MixHash)
                            ?? MixHash.new-from-pairs(@p.shift.pairs)
                            !! @p.shift.MixHash;
        for @p.map(*.Mix(:view)) -> $mix {
            my $i = $mixhash (|) $mix;
            for $i.keys -> $k {
                $mixhash{$k}  = $mixhash{$k}
                                  ?? abs($mixhash{$k} - $mix{$k})
                                  !! $mix{$k};
            }
        }
        $mixhash.Mix(:view);
    } else {
        my $baghash = nqp::istype(@p[0],BagHash)
                ?? BagHash.new-from-pairs(@p.shift.pairs)
                !! @p.shift.BagHash;
        for @p.map(*.Bag(:view)) -> $bag {
            my $i = $baghash (|) $bag;
            for $i.keys -> $k {
                $baghash{$k}  = $baghash{$k}
                                  ?? abs($baghash{$k} - $bag{$k})
                                  !! $bag{$k};
            }
        }
        $baghash.Bag(:view);
    }
}
# U+2296 CIRCLED MINUS
only sub infix:<<"\x229c">>(|p) {
    infix:<(^+)>(|p);
}

# TODO: polymorphic eqv
# multi sub infix:<eqv>(Any $a, Any $b --> Bool) {
#     $a.Set(:view) eqv $b.Set(:view);
# }
# multi sub infix:<eqv>(Setty $a, Setty $b --> Bool) {
#     $a == $b and so $a.keys.all (elem) $b
# }

proto sub infix:<<(<=)>>($, $ --> Bool) is pure {*}
multi sub infix:<<(<=)>>(Any $a, Any $b --> Bool) {
    $a.Set(:view) (<=) $b.Set(:view);
}
multi sub infix:<<(<=)>>(Setty $a, Setty $b --> Bool) {
    $a <= $b and so $a.keys.all (elem) $b
}
# U+2286 SUBSET OF OR EQUAL TO
only sub infix:<⊆>($a, $b --> Bool) is pure {
    $a (<=) $b;
}
# U+2288 NEITHER A SUBSET OF NOR EQUAL TO
only sub infix:<⊈>($a, $b --> Bool) is pure {
    $a !(<=) $b;
}

proto sub infix:<<(<)>>($, $ --> Bool) is pure {*}
multi sub infix:<<(<)>>(Any $a, Any $b --> Bool) {
    $a.Set(:view) (<) $b.Set(:view);
}
multi sub infix:<<(<)>>(Setty $a, Setty $b --> Bool) {
    $a < $b and so $a.keys.all (elem) $b;
}
# U+2282 SUBSET OF
only sub infix:<⊂>($a, $b --> Bool) is pure {
    $a (<) $b;
}
# U+2284 NOT A SUBSET OF
only sub infix:<⊄>($a, $b --> Bool) is pure {
    $a !(<) $b;
}

proto sub infix:<<(>=)>>($, $ --> Bool) is pure {*}
multi sub infix:<<(>=)>>(Any $a, Any $b --> Bool) {
    $a.Set(:view) (>=) $b.Set(:view);
}
multi sub infix:<<(>=)>>(Setty $a, Setty $b --> Bool) {
    $a >= $b and so $b.keys.all (elem) $a;
}
# U+2287 SUPERSET OF OR EQUAL TO
only sub infix:<⊇>($a, $b --> Bool) is pure {
    $a (>=) $b;
}
# U+2289 NEITHER A SUPERSET OF NOR EQUAL TO
only sub infix:<⊉>($a, $b --> Bool) is pure {
    $a !(>=) $b;
}

proto sub infix:<<(>)>>($, $ --> Bool) is pure {*}
multi sub infix:<<(>)>>(Any $a, Any $b --> Bool) {
    $a.Set(:view) (>) $b.Set(:view);
}
multi sub infix:<<(>)>>(Setty $a, Setty $b --> Bool) {
    $a > $b and so $b.keys.all (elem) $a;
}
# U+2283 SUPERSET OF
only sub infix:<⊃>($a, $b --> Bool) is pure {
    $a (>) $b;
}
# U+2285 NOT A SUPERSET OF
only sub infix:<⊅>($a, $b --> Bool) is pure {
    $a !(>) $b;
}

only sub infix:<(.)>(**@p) is pure {
    return bag() unless @p;

    if @p.first(Mixy) {
        my $mixhash = nqp::istype(@p[0], MixHash)
            ?? MixHash.new-from-pairs(@p.shift.pairs)
            !! @p.shift.MixHash;
        for @p.map(*.Mix(:view)) -> $mix {
            $mix{$_}
              ?? ($mixhash{$_} *= $mix{$_})
              !! $mixhash.DELETE-KEY($_)
              for $mixhash.keys;
        }
        $mixhash.Mix(:view);
    } else {  # go Baggy by default
        my $baghash = nqp::istype(@p[0], BagHash)
            ?? BagHash.new-from-pairs(@p.shift.pairs)
            !! @p.shift.BagHash;
        for @p.map(*.Bag(:view)) -> $bag {
            $bag{$_}
              ?? ($baghash{$_} *= $bag{$_})
              !! $baghash.DELETE-KEY($_)
              for $baghash.keys;
        }
        $baghash.Bag(:view);
    }
}
# U+228D MULTISET MULTIPLICATION
only sub infix:<⊍>(|p) is pure {
    infix:<(.)>(|p);
}

only sub infix:<(+)>(**@p) is pure {
    return bag() unless @p;

    if @p.first(Mixy) {
        my $mixhash = nqp::istype(@p[0], MixHash)
            ?? MixHash.new-from-pairs(@p.shift.pairs)
            !! @p.shift.MixHash;
        for @p.map(*.Mix(:view)) -> $mix {
            $mixhash{$_} += $mix{$_} for $mix.keys;
        }
        $mixhash.Mix(:view);
    } else {  # go Baggy by default
        my $baghash = nqp::istype(@p[0], BagHash)
            ?? BagHash.new-from-pairs(@p.shift.pairs)
            !! @p.shift.BagHash;
        for @p.map(*.Bag(:view)) -> $bag {
            $baghash{$_} += $bag{$_} for $bag.keys;
        }
        $baghash.Bag(:view);
    }
}
# U+228E MULTISET UNION
only sub infix:<⊎>(|p) is pure {
    infix:<(+)>(|p);
}

proto sub infix:<<(<+)>>($, $ --> Bool) is pure {*}
multi sub infix:<<(<+)>>(Any $a, Any $b --> Bool) {
    if nqp::istype($a, Mixy) or nqp::istype($b, Mixy) {
        $a.Mix(:view) (<+) $b.Mix(:view);
    } else {
        $a.Bag(:view) (<+) $b.Bag(:view);
    }
}
multi sub infix:<<(<+)>>(QuantHash $a, QuantHash $b --> Bool) {
    for $a.keys {
        return False if $a{$_} > $b{$_};
    }
    True;
}
# U+227C PRECEDES OR EQUAL TO
only sub infix:<≼>($a, $b --> Bool) is pure {
    $a (<+) $b;
}

proto sub infix:<<(>+)>>($, $ --> Bool) is pure {*}
multi sub infix:<<(>+)>>(Baggy $a, Baggy $b --> Bool) {
    for $b.keys {
        return False if $b{$_} > $a{$_};
    }
    True;
}
multi sub infix:<<(>+)>>(Any $a, Any $b --> Bool) {
    if nqp::istype($a, Mixy) or nqp::istype($b, Mixy) {
        $a.Mix(:view) (>+) $b.Mix(:view);
    } else {
        $a.Bag(:view) (>+) $b.Bag(:view);
    }
}
# U+227D SUCCEEDS OR EQUAL TO
only sub infix:<≽>($a, $b --> Bool) is pure {
    $a (>+) $b;
}

sub set(*@a --> Set) { Set.new(@a) }
sub bag(*@a --> Bag) { Bag.new(@a) }
sub mix(*@a --> Mix) { Mix.new(@a) }

# vim: ft=perl6 expandtab sw=4
