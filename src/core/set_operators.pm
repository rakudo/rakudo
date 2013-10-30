
proto sub infix:<(elem)>($, $ --> Bool) {*}
multi sub infix:<(elem)>($a, Any $b --> Bool) {
    $a (elem) $b.Set(:view);
}
multi sub infix:<(elem)>($a, Set $b --> Bool) {
    $b.exists_key($a);
}
# U+2208 ELEMENT OF
only sub infix:<<"\x2208">>($a, $b --> Bool) {
    $a (elem) $b;
}
# U+2209 NOT AN ELEMENT OF
only sub infix:<<"\x2209">>($a, $b --> Bool) {
    $a !(elem) $b;
}

proto sub infix:<(cont)>($, $ --> Bool) {*}
multi sub infix:<(cont)>(Any $a, $b --> Bool) {
    $a.Set(:view) (cont) $b;
}
multi sub infix:<(cont)>(Set $a, $b --> Bool) {
    $a.exists_key($b);
}
# U+220B CONTAINS AS MEMBER
only sub infix:<<"\x220B">>($a, $b --> Bool) {
    $a (cont) $b;
}
# U+220C DOES NOT CONTAIN AS MEMBER
only sub infix:<<"\x220C">>($a, $b --> Bool) {
    $a !(cont) $b;
}

only sub infix:<(|)>(**@p) {
    if @p.grep(Baggy) {
        my $baghash = BagHash.new;
        for @p.map(*.Bag(:view)) -> $bag {
            $baghash{$_} max= $bag{$_} for $bag.keys;
        }
        $baghash.Bag(:view);
    }
    else {
        Set.new( @p.map(*.Set(:view).keys) );
    }
}
# U+222A UNION
only sub infix:<<"\x222A">>(|p) {
    infix:<(|)>(|p);
}

only sub infix:<(&)>(**@p) {
    return set() unless @p;

    if @p.grep(Baggy) {
        my $baghash = @p[0] ~~ BagHash
          ?? BagHash.new-fp(@p.shift.pairs)
          !! @p.shift.BagHash;
        for @p.map(*.Bag(:view)) -> $bag {
            $bag{$_}
              ?? $baghash{$_} min= $bag{$_}
              !! $baghash.delete_key($_)
              for $baghash.keys;
        }
        $baghash.Bag(:view);
    }
    else {
        my $sethash = @p[0] ~~ SetHash
          ?? SetHash.new(@p.shift.keys)
          !! @p.shift.SetHash;
        for @p.map(*.Set(:view)) -> $set {
            $set{$_} || $sethash.delete_key($_) for $sethash.keys;
        }
        $sethash.Set(:view);
    }
}
# U+2229 INTERSECTION
only sub infix:<<"\x2229">>(|p) {
    infix:<(&)>(|p);
}

only sub infix:<(-)>(**@p) {
    return set() unless @p;

    if @p[0] ~~ Baggy {
        my $baghash = @p[0] ~~ BagHash
          ?? BagHash.new-fp(@p.shift.pairs)
          !! @p.shift.BagHash;
        for @p.map(*.Bag(:view)) -> $bag {
            $bag{$_} < $baghash{$_}
              ?? $baghash{$_} -= $bag{$_}
              !! $baghash.delete_key($_)
              for $baghash.keys;
        }
        $baghash.Bag(:view);
    }
    else {
        my $sethash = @p[0] ~~ SetHash
          ?? SetHash.new(@p.shift.keys)
          !! @p.shift.SetHash;
        for @p.map(*.Set(:view)) -> $set {
            $set{$_} && $sethash.delete_key($_) for $sethash.keys;
        }
        $sethash.Set(:view);
    }
}
# U+2216 SET MINUS
only sub infix:<<"\x2216">>(|p) {
    infix:<(-)>(|p);
}

proto sub infix:<(^)>($, $ --> Setty) {*}
multi sub infix:<(^)>(Any $a, Any $b --> Setty) {
    $a.Set(:view) (^) $b.Set(:view);
}
multi sub infix:<(^)>(Set $a, Set $b --> Setty) {
    ($a (-) $b) (|) ($b (-) $a);
}
# U+2296 CIRCLED MINUS
only sub infix:<<"\x2296">>($a, $b --> Setty) {
    $a (^) $b;
}

# TODO: polymorphic eqv
# multi sub infix:<eqv>(Any $a, Any $b --> Bool) {
#     $a.Set(:view) eqv $b.Set(:view);
# }
# multi sub infix:<eqv>(Setty $a, Setty $b --> Bool) {
#     $a == $b and so $a.keys.all (elem) $b
# }

proto sub infix:<<(<=)>>($, $ --> Bool) {*}
multi sub infix:<<(<=)>>(Any $a, Any $b --> Bool) {
    $a.Set(:view) (<=) $b.Set(:view);
}
multi sub infix:<<(<=)>>(Setty $a, Setty $b --> Bool) {
    $a <= $b and so $a.keys.all (elem) $b
}
# U+2286 SUBSET OF OR EQUAL TO
only sub infix:<<"\x2286">>($a, $b --> Bool) {
    $a (<=) $b;
}
# U+2288 NEITHER A SUBSET OF NOR EQUAL TO
only sub infix:<<"\x2288">>($a, $b --> Bool) {
    $a !(<=) $b;
}

proto sub infix:<<(<)>>($, $ --> Bool) {*}
multi sub infix:<<(<)>>(Any $a, Any $b --> Bool) {
    $a.Set(:view) (<) $b.Set(:view);
}
multi sub infix:<<(<)>>(Setty $a, Setty $b --> Bool) {
    $a < $b and so $a.keys.all (elem) $b;
}
# U+2282 SUBSET OF
only sub infix:<<"\x2282">>($a, $b --> Bool) {
    $a (<) $b;
}
# U+2284 NOT A SUBSET OF
only sub infix:<<"\x2284">>($a, $b --> Bool) {
    $a !(<) $b;
}

proto sub infix:<<(>=)>>($, $ --> Bool) {*}
multi sub infix:<<(>=)>>(Any $a, Any $b --> Bool) {
    $a.Set(:view) (>=) $b.Set(:view);
}
multi sub infix:<<(>=)>>(Setty $a, Setty $b --> Bool) {
    $a >= $b and so $b.keys.all (elem) $a;
}
# U+2287 SUPERSET OF OR EQUAL TO
only sub infix:<<"\x2287">>($a, $b --> Bool) {
    $a (>=) $b;
}
# U+2289 NEITHER A SUPERSET OF NOR EQUAL TO
only sub infix:<<"\x2289">>($a, $b --> Bool) {
    $a !(>=) $b;
}

proto sub infix:<<(>)>>($, $ --> Bool) {*}
multi sub infix:<<(>)>>(Any $a, Any $b --> Bool) {
    $a.Set(:view) (>) $b.Set(:view);
}
multi sub infix:<<(>)>>(Setty $a, Setty $b --> Bool) {
    $a > $b and so $b.keys.all (elem) $a;
}
# U+2283 SUPERSET OF
only sub infix:<<"\x2283">>($a, $b --> Bool) {
    $a (>) $b;
}
# U+2285 NOT A SUPERSET OF
only sub infix:<<"\x2285">>($a, $b --> Bool) {
    $a !(>) $b;
}

only sub infix:<(.)>(**@p) {
    my $baghash = @p[0] ~~ BagHash
      ?? BagHash.new-fp(@p.shift.pairs)
      !! @p.shift.BagHash;
    for @p.map(*.Bag(:view)) -> $bag {
        $bag{$_}
          ?? $baghash{$_} *= $bag{$_}
          !! $baghash.delete_key($_)
          for $baghash.keys;
    }
    $baghash.Bag(:view);
}
# U+228D MULTISET MULTIPLICATION
only sub infix:<<"\x228D">>(|p) {
    infix:<(.)>(|p);
}

only sub infix:<(+)>(**@p) {
    return bag() unless @p;

    my $baghash = @p[0] ~~ BagHash
      ?? BagHash.new-fp(@p.shift.pairs)
      !! @p.shift.BagHash;
    for @p.map(*.Bag(:view)) -> $bag {
        $baghash{$_} += $bag{$_} for $bag.keys;
    }
    $baghash.Bag(:view);
}
# U+228E MULTISET UNION
only sub infix:<<"\x228E">>(|p) {
    infix:<(+)>(|p);
}

proto sub infix:<<(<+)>>($, $ --> Bool) {*}
multi sub infix:<<(<+)>>(Any $a, Any $b --> Bool) {
    $a.Bag(:view) (<+) $b.Bag(:view);
}
multi sub infix:<<(<+)>>(Baggy $a, Baggy $b --> Bool) {
    so all $a.keys.map({ $a{$_} <= $b{$_} })
}
# U+227C PRECEDES OR EQUAL TO
only sub infix:<<"\x227C">>($a, $b --> Bool) {
    $a (<+) $b;
}
  
proto sub infix:<<(>+)>>($, $ --> Bool) {*}
multi sub infix:<<(>+)>>(Baggy $a, Baggy $b --> Bool) {
    so all $b.keys.map({ $b{$_} <= $a{$_} });
}
multi sub infix:<<(>+)>>(Any $a, Any $b --> Bool) {
    $a.Bag(:view) (>+) $b.Bag(:view);
}
# U+227D SUCCEEDS OR EQUAL TO
only sub infix:<<"\x227D">>($a, $b --> Bool) {
    $a (>+) $b;
}

sub set(*@args --> Set) { Set.new(@args) }
sub bag(*@args --> Bag) { Bag.new(|@args) }
sub mix(*@args --> Mix) { Mix.new(|@args) }
# U+2205 EMPTY SET
#constant term:<<"\x2205">> = set();  #Cannot call ACCEPTS; no signatures match
