my class KeyBag does Associative does Baggy {
    has %!elems; # should be UInt

    method default { 0 }
    method keys { %!elems.keys }
    method values { %!elems.values }
    method elems returns Int { [+] self.values }
    method exists($k) returns Bool { %!elems.exists($k) }
    method delete($k) { %!elems.delete($k) }
    method Bool { %!elems.Bool }
    method Numeric { self.elems }
    method Real { self.elems }
    method hash { %!elems.hash }
    method Set { set self.keys }
    method KeySet { KeySet.new(self.keys) }
    method Bag { Bag.new-from-pairs(self.hash) }
    method KeyBag { self }

    method at_key($k) {
        Proxy.new(
          FETCH => {
              %!elems{$k} // 0;
          },
          STORE => -> $, $value {
              if $value > 0 {
                  %!elems{$k} = $value;
              }
              else {
                  %!elems.delete($k);
              }
          }
        );
    }

    # Constructor
    method new(*@args --> KeyBag) {
        my %e;
        %e{$_}++ for @args;
        self.bless(:elems(%e));
    }
    method new-from-pairs(*@pairs --> KeyBag) {
        my %e;
        for @pairs {
            when Pair { %e{.key} = .value + (%e{.key} // 0); }
            %e{$_}++;
        }
        for %e -> $p {
            die "Negative values are not allowed in KeyBags" if $p.value < 0;
            %e.delete($p.key) if $p.value == 0;
        }
        self.bless(:elems(%e));
    }

    submethod BUILD (:%!elems) { }

    method ACCEPTS($other) {
        self.defined
          ?? $other (<+) self && self (<+) $other
          !! $other.^does(self);
    }

    multi method Str(Bag:D:) { ~ self.pairs.map: { .key xx .value } }
    multi method gist(Any:D $ : --> Str) { "keybag({ self.pairs>>.gist.join(', ') })" }
    multi method perl(Any:D $ : --> Str) {
        self.defined
          ?? %!elems.perl ~ '.KeyBag'
          !! "KeyBag";
    }

    method iterator() { %!elems.pairs.iterator }
    method list() { %!elems.keys }
    method pairs() { %!elems.pairs }

    method pick($count = 1) {
        return self.roll if $count ~~ Num && $count == 1;

        my $temp-bag = KeyBag.new-from-pairs(self.hash);
        my $lc = $count ~~ Whatever ?? Inf !! $count;
        gather while $temp-bag && $lc-- {
            my $choice = $temp-bag.roll;
            take $choice;
            $temp-bag{$choice}--;
        }
    }
    method roll($count = 1) {
        my @inverse-mapping;
        my $a = 0;
        for %!elems.pairs -> $pair {
            my $b = $a + $pair.value;
            @inverse-mapping.push(($a..^$b) => $pair.key);
            $a = $b;
        }

        sub choose {
            my $choice = $a.rand;
            my $i = 0;
            for @inverse-mapping -> $im {
                if $choice ~~ $im.key {
                    return $im.value;
                }
            }
            fail "Problem with KeyBag.roll";
        }

        return choose() xx * if $count ~~ Whatever;
        return choose() if $count == 1;
        return choose() xx $count;
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

sub keybag(*@a) returns KeyBag {
    KeyBag.new(|@a);
}
