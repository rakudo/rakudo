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

    multi method Str(Any:D $ : --> Str) { "bag({ self.pairs>>.perl.join(', ') })" }
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

