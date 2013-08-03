my class KeyBag does Associative does Baggy {
    has %!elems; # should be UInt

    method default { 0 }
    method keys { %!elems.keys }
    method values { %!elems.values }
    method elems returns Int { [+] self.values }
    method exists($a) returns Bool { %!elems.exists($a) }
    method Bool { %!elems.Bool }
    method Numeric { self.elems }
    method Real { self.elems }
    method hash { %!elems.hash }
    method at_key($k) {
        Proxy.new(FETCH => { %!elems.exists($k) ?? %!elems{$k} !! 0 },
                  STORE => -> $, $value { if $value > 0 { %!elems{$k} = $value } else { %!elems.delete($k) }});
    }
    method exists_key($k) { self.exists($k) }
    method delete_key($k) { %!elems.delete($k) }

    # Constructor
    method new(*@args --> KeyBag) {
        my %e;
        sub register-arg($arg) {
            given $arg {
                when Pair { if .value { if %e.exists(.key) { %e{.key} += .value } else { %e{.key} = .value } } }
                when Set | KeySet { for .keys -> $key { %e{$key}++; } }
                when Associative { for .pairs -> $p { register-arg($p) } }
                when Positional { for .list -> $p { register-arg($p) } }
                default { %e{$_}++; }
            }
        }

        for @args {
            register-arg($_);
        }
        self.bless(*, :elems(%e));
    }

    submethod BUILD (:%!elems) { }

    multi method Str(Bag:D:) { ~ self.pairs.map: { .key xx .value } }
    multi method gist(Any:D $ : --> Str) { "keybag({ self.pairs>>.gist.join(', ') })" }
    multi method perl(Any:D $ : --> Str) { 'KeyBag.new(' ~ %!elems.perl ~ ')' }

    method iterator() { %!elems.pairs.iterator }
    method list() { %!elems.keys }
    method pairs() { %!elems.pairs }

    method push(*@values) {
        my $previous;
        my $has_previous;
        for @values -> $e {
            if $has_previous {
                self{$previous} += $e;
                $has_previous = 0;
            }
            elsif $e.^isa(Enum) {
                self{$e.key} += $e.value;
            }
            else {
                $previous = $e;
                $has_previous = 1;
            }
        }
        if $has_previous {
            warn "Trailing item in KeyBag.push";
        }
        self
    }

    method pick($count = 1) {
        return self.roll if $count ~~ Num && $count == 1;

        my $temp-bag = KeyBag.new(self);
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

    proto method classify_list(|) { * }
    multi method classify_list( &test, *@list ) {
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
    multi method classify_list( %test, *@list ) {
        fail 'Cannot .classify an infinite list' if @list.infinite;
        if @list {

            # multi-level classify
            if %test{@list[0]} ~~ List {
                for @list -> $l {
                    my @keys  = %test{$l};
                    my $last := @keys.pop;
                    my $bag   = self;
                    $bag = $bag{$_} //= self.new for @keys;
                    $bag{$last}++;
                }
            }

            # just a simple classify
            else {
                self{%test{$_}}++ for @list;
            }
        }
        self;
    }
    multi method classify_list( @test, *@list ) {
        fail 'Cannot .classify an infinite list' if @list.infinite;
        if @list {

            # multi-level classify
            if @test[@list[0]] ~~ List {
                for @list -> $l {
                    my @keys  = @test[$l];
                    my $last := @keys.pop;
                    my $bag   = self;
                    $bag = $bag{$_} //= self.new for @keys;
                    $bag{$last}++;
                }
            }

            # just a simple classify
            else {
                self{@test[$_]}++ for @list;
            }
        }
        self;
    }

    proto method categorize_list(|) { * }
    multi method categorize_list( &test, *@list ) {
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
    multi method categorize_list( %test, *@list ) {
        fail 'Cannot .categorize an infinite list' if @list.infinite;
        if @list {

            # multi-level categorize
            if %test{@list[0]}[0] ~~ List {
                for @list -> $l {
                    for %test{$l} -> $k {
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
                    self{$_}++ for %test{$l};
                }
            }
        }
        self;
    }
    multi method categorize_list( @test, *@list ) {
        fail 'Cannot .categorize an infinite list' if @list.infinite;
        if @list {

            # multi-level categorize
            if @test[@list[0]][0] ~~ List {
                for @list -> $l {
                    for @test[$l] -> $k {
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
                    self{$_}++ for @test[$l];
                }
            }
        }
        self;
    }
}
