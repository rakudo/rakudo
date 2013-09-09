my role Baggy does Associative {
    has %!elems;
#         |-- key.WHICH
#              |-- (key => value)

#- specific methods for users of role ------------------------------------------
    method BUILD (:%!elems) {}

    method at_key($k) {
        Proxy.new(
          FETCH => {
              my $key   := $k.WHICH;
              my $elems := nqp::getattr(self, KeyBag, '%!elems');
              $elems.exists($key) ?? $elems{$key}.value !! 0;
          },
          STORE => -> $, $value {
              if $value > 0 {
                  (nqp::getattr(self, KeyBag, '%!elems'){$k.WHICH}
                    //= ($k => 0)).value = $value;
              }
              elsif $value == 0 {
                  self.delete($k);
              }
              else {
                  fail "Cannot put negative value $value for $k in {self.^name}";
              }
              $value;
          }
        );
    }

    method delete($k) {
        my $key   := $k.WHICH;
        my $elems := nqp::getattr(self, KeyBag, '%!elems');
        if $elems.exists($key) {
            my $value = $elems{$key}.value;
            $elems.delete($key);
            $value;
        }
        else {
            0;
        }
    }
#-------------------------------------------------------------------------------

    method default(--> Int) { 0 }
    method keys { %!elems.values.map( {.key} ) }
    method values { %!elems.values.map( {.value} ) }
    method elems(--> Int) { [+] self.values }
    method exists($k --> Bool) { %!elems.exists($k.WHICH) }
    method Bool { %!elems.Bool }
    method Numeric { self.elems }
    method Real { self.elems }

    method hash(--> Hash) { %!elems.values.hash }

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
            %e.delete($p.key)         if $pair.value <= 0;
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

    method pick($count = 1) {
        %!elems.values.map({ .key xx .value }).pick($count);
    }
    method roll($count = 1) {
        %!elems.values.map({ .key xx .value }).roll($count);
    }

#    method pick($count = 1) {
#        return self.roll if $count ~~ Num && $count == 1;
#
#        my $temp-bag = KeyBag.new-fp(self.hash);
#        my $lc = $count ~~ Whatever ?? Inf !! $count;
#        gather while $temp-bag && $lc-- {
#            my $choice = $temp-bag.roll;
#            take $choice;
#            $temp-bag{$choice}--;
#        }
#    }
#    method roll($count = 1) {
#        my @inverse-mapping;
#        my $a = 0;
#        for %!elems.pairs -> $pair {
#            my $b = $a + $pair.value;
#            @inverse-mapping.push(($a..^$b) => $pair.key);
#            $a = $b;
#        }
#
#        sub choose {
#            my $choice = $a.rand;
#            my $i = 0;
#            for @inverse-mapping -> $im {
#                if $choice ~~ $im.key {
#                    return $im.value;
#                }
#            }
#            fail "Problem with KeyBag.roll";
#        }
#
#        return choose() xx * if $count ~~ Whatever;
#        return choose() if $count == 1;
#        return choose() xx $count;
#    }

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
    my $set = Set.new: @p.map(*.Set(:view).keys);
    my @bags = @p.map(*.Bag(:view));
    Bag.new-fp($set.map({ ; $_ => [*] @bags>>.{$_} }));
}
# U+228D MULTISET MULTIPLICATION
only sub infix:<<"\x228D">>(|p) {
    infix:<(.)>(|p);
}

only sub infix:<(+)>(**@p) {
    my $set = Set.new: @p.map(*.Set(:view).keys);
    my @bags = @p.map(*.Bag(:view));
    Bag.new-fp($set.map({ ; $_ => [+] @bags>>.{$_} }));
}
# U+228E MULTISET UNION
only sub infix:<<"\x228E">>(|p) {
    infix:<(+)>(|p);
}

proto sub infix:<<(<+)>>($, $ --> Bool) {*}
multi sub infix:<<(<+)>>(Any $a, Any $b --> Bool) {
    $a.Bag (<+) $b.Bag;
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
    $a.Bag (>+) $b.Bag;
}
# U+227D SUCCEEDS OR EQUAL TO
only sub infix:<<"\x227D">>($a, $b --> Bool) {
    $a (>+) $b;
}

sub bag(*@args --> Bag) { Bag.new(|@args) }
