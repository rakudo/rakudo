my role Baggy does QuantHash {
    has %!elems;  # key.WHICH => (key=>value)

    method BUILD (:%!elems) {}
    method default(--> Int) { 0 }
    method keys { %!elems.values.map( {.key} ) }
    method values { %!elems.values.map( {.value} ) }
    method elems(--> Int) { [+] self.values }
    method exists ($k --> Bool) {  # is DEPRECATED doesn't work in settings
        once DEPRECATED("Method 'Baggy.exists'","the :exists adverb");
        self.exists_key($k);
    }
    method exists_key($k --> Bool) {
        %!elems.exists_key($k.WHICH);
    }
    method Bool { %!elems.Bool }
    method Numeric { self.elems }
    method Real { self.elems }

    method hash(--> Hash) { %!elems.values.hash }
    method invert(--> List) { %!elems.values.map: { ( .value => .key ) } }

    method new(*@args --> Baggy) {
        my %e;
        # need explicit signature because of #119609
        -> $_ { (%e{$_.WHICH} //= ($_ => 0)).value++ } for @args;
        self.bless(:elems(%e));
    }
    method new-fp(*@pairs --> Baggy) {
        my %e;
        for @pairs {
            when Pair {
                (%e{$_.key.WHICH} //= ($_.key => 0)).value += $_.value;
            }
            default {
                (%e{$_.WHICH} //= ($_ => 0)).value++;
            }
        }
        my @toolow;
        for %e -> $p {
            my $pair := $p.value;
            @toolow.push( $pair.key ) if $pair.value <  0;
            %e.delete_key($p.key)     if $pair.value <= 0;
        }
        fail "Found negative values for {@toolow} in {self.^name}" if @toolow;
        self.bless(:elems(%e));
    }

    method ACCEPTS($other) {
        self.defined
          ?? $other (<+) self && self (<+) $other
          !! $other.^does(self);
    }

    multi method Str(Baggy:D $ : --> Str) { ~ self.pairs.map({ .key xx .value }) }
    multi method gist(Baggy:D $ : --> Str) {
        my $name := self.^name;
        ( $name eq 'Bag' ?? 'bag' !! "$name.new" )
        ~ '('
        ~ %!elems.values.map( {
            .value > 1  # rather arbitrarily
              ?? "{.key.gist}({.value})"
              !! .key.gist xx .value
        } ).join(', ')
        ~ ')';
    }
    multi method perl(Baggy:D $ : --> Str) {
        "{self.^name}.new-fp("
        ~ %!elems.values.map( {"{.key.perl}=>{.value}"} ).join(',')
        ~ ')';
    }

    method list() { self.keys }
    method pairs() { %!elems.values }

    method pick ($count = 1) {
        return self.roll if $count ~~ Num && $count == 1;

        my $elems = self.elems;
        my $picks = $elems min $count;
        my @pairs = self.pairs.map( { $_.key => $_.value } );;

        map {
            my $rand = $elems.rand.Int;
            my $seen = 0;
            my $pick;
            for @pairs -> $pair {
                next if ( $seen += $pair.value ) <= $rand;

                $pick = $pair.key;
                $pair.value--;
                $elems--;
                last;
            }
            $pick;
        }, 1 .. $picks;
    }

    method roll ($count = 1) {
        my $elems  = self.elems;
        my $rolls  = $count ~~ Num ?? $elems min $count !! $count;
        my @pairs := self.pairs;

        map {
            my $rand = $elems.rand.Int;
            my $seen = 0;
            my $roll;
            for @pairs -> $pair {
                next if ( $seen += $pair.value ) <= $rand;

                $roll = $pair.key;
                last;
            }
            $roll;
        }, 1 .. $rolls;
    }

    proto method classify-list(|) { * }
    multi method classify-list( &test, *@list ) {
        fail 'Cannot .classify an infinite list' if @list.infinite;
        if @list {

            # multi-level classify
            if test(@list[0]) ~~ List {
                for @list -> $l {
                    my @keys  = test($l);
                    my $last := @keys.pop;
                    my $bag   = self;
                    $bag = $bag{$_} //= self.new for @keys;
                    $bag{$last}++;
                }
            }

            # just a simple classify
            else {
                self{test $_}++ for @list;
            }
        }
        self;
    }
    multi method classify-list( %test, *@list ) {
        samewith( { %test{$^a} }, @list );
    }
    multi method classify-list( @test, *@list ) {
        samewith( { @test[$^a] }, @list );
    }

    proto method categorize-list(|) { * }
    multi method categorize-list( &test, *@list ) {
        fail 'Cannot .categorize an infinite list' if @list.infinite;
        if @list {

            # multi-level categorize
            if test(@list[0])[0] ~~ List {
                for @list -> $l {
                    for test($l) -> $k {
                        my @keys  = @($k);
                        my $last := @keys.pop;
                        my $bag   = self;
                        $bag = $bag{$_} //= self.new for @keys;
                        $bag{$last}++;
                    }
                }
            }

            # just a simple categorize
            else {
                for @list -> $l {
                    self{$_}++ for test($l);
                }
            }
        }
        self;
    }
    multi method categorize-list( %test, *@list ) {
        samewith( { %test{$^a} }, @list );
    }
    multi method categorize-list( @test, *@list ) {
        samewith( { @test[$^a] }, @list );
    }
}

only sub infix:<(.)>(**@p) {
    my $keybag = @p[0] ~~ BagHash
      ?? BagHash.new-fp(@p.shift.pairs)
      !! @p.shift.BagHash;
    for @p.map(*.Bag(:view)) -> $bag {
        $bag{$_}
          ?? $keybag{$_} *= $bag{$_}
          !! $keybag.delete_key($_)
          for $keybag.keys;
    }
    $keybag.Bag(:view);
}
# U+228D MULTISET MULTIPLICATION
only sub infix:<<"\x228D">>(|p) {
    infix:<(.)>(|p);
}

only sub infix:<(+)>(**@p) {
    return bag() unless @p;

    my $keybag = @p[0] ~~ BagHash
      ?? BagHash.new-fp(@p.shift.pairs)
      !! @p.shift.BagHash;
    for @p.map(*.Bag(:view)) -> $bag {
        $keybag{$_} += $bag{$_} for $bag.keys;
    }
    $keybag.Bag(:view);
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

sub bag(*@args --> Bag) { Bag.new(|@args) }
