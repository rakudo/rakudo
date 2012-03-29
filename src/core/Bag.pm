my role Baggy { Any }

my class Bag is Iterable does Associative does Baggy {
    has %!elems; # should be UInt

    method keys { %!elems.keys }
    method values { %!elems.values }
    method elems returns Int { [+] self.values }
    method exists($a) returns Bool { %!elems.exists($a) }
    method Bool { %!elems.Bool }
    method Numeric { self.elems }
    method hash { %!elems.hash }
    method at_key($k) { +(%!elems{$k} // 0) }
    method exists_key($k) { self.exists($k) }

    # Constructor
    method new(*@args --> Bag) {
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

    multi method Str(Any:D $ : --> Str) { ~ self.pairs.map: { .key xx .value } }
    multi method gist(Any:D $ : --> Str) { "bag({ self.pairs>>.gist.join(', ') })" }
    multi method perl(Any:D $ : --> Str) { 'Bag.new(' ~ %!elems.perl ~ ')' }

    method iterator() { %!elems.pairs.iterator }
    method list() { %!elems.keys }
    method pairs() { %!elems.pairs }

    method pick($count = 1) { my $kb = KeyBag.new(self); $kb.pick($count); }
    method roll($count = 1) { my $kb = KeyBag.new(self); $kb.roll($count); }
}

sub bag(*@a) returns Bag {
    Bag.new(|@a);
}

my class KeyBag does Associative does Baggy {
    has %!elems; # should be UInt

    method keys { %!elems.keys }
    method values { %!elems.values }
    method elems returns Int { [+] self.values }
    method exists($a) returns Bool { %!elems.exists($a) }
    method Bool { %!elems.Bool }
    method Numeric { self.elems }
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
            $a += $pair.value;
            @inverse-mapping.push((+$a) => $pair.key);
        }

        sub choose {
            my $choice = $a.rand;
            my $i = 0;
            for @inverse-mapping -> $im {
                if $choice ~~ $i ..^ +$im.key {
                    return $im.value;
                }
                $i = $im.key;
            }
        }

        return choose() xx * if $count ~~ Whatever;
        return choose() if $count == 1;
        return choose() xx $count;
    }
}
