my class Set is Iterable does Associative {
    has %!elems;

    method keys { %!elems.keys }
    method values { %!elems.values }
    method elems returns Int { %!elems.elems }
    method exists($a) returns Bool { %!elems.exists($a) }
    method Bool { %!elems.Bool }
    method Numeric { %!elems.Numeric }
    method hash { %!elems.hash }
    method at_key($k) { ?(%!elems{$k} // False) }
    method exists_key($k) { self.exists($k) }

    # Constructor
    method new(*@args --> Set) {
        my %e;
        sub register-arg($arg) {
            given $arg {
                when Pair { %e{.key} = True; }
                when Set | KeySet { for .keys -> $key { %e{$key} = True; } }
                when Associative { for .pairs -> $p { register-arg($p); } }
                when Positional { for .list -> $p { register-arg($p); } }
                default { %e{$_} = True; }
            }
        }

        for @args {
            register-arg($_);
        }
        self.bless(*, :elems(%e));
    }

    submethod BUILD (:%!elems) { }

    # Coercions to and from
    method postcircumfix:<( )> ($s --> Set) { to-set($s) }
    multi to-set (Set $set --> Set) { $set }
    multi to-set (KeySet $set --> Set) { Set.new: $set }
    multi to-set (Bag $bag --> Set) { Set.new: $bag }
    multi to-set (KeyBag $bag --> Set) { Set.new: $bag }
    multi to-set (@elems --> Set) { Set.new: @elems }
    multi to-set ([*@elems] --> Set) { Set.new: @elems }
    multi to-set (%elems --> Set) { Set.new: %elems.keys }
    multi to-set ($elem --> Set) { die "Cannot coerce $elem.perl() to a Set; use set($elem.perl()) to create a one-element set" }

    multi method Str(Any:D $ : --> Str) { ~%!elems.keys() }
    multi method gist(Any:D $ : --> Str) { "set({ %!elems.keys».gist.join(', ') })"
    }
    multi method perl(Any:D $ : --> Str) { 'set(' ~ join(', ', map { .perl }, %!elems.keys) ~ ')' }

    method iterator() { %!elems.keys.iterator }
    method list() { %!elems.keys }
    method pick($count = 1) { %!elems.keys.pick($count) }
    method roll($count = 1) { %!elems.keys.roll($count) }

    # TODO: WHICH will require the capability for >1 pointer in ObjAt
}

sub set(*@args) {
    Set.new(@args);
}

my class KeySet is Iterable does Associative {
    has %!elems;

    method keys { %!elems.keys }
    method values { %!elems.values }
    method elems returns Int { %!elems.elems }
    method exists($a) returns Bool { %!elems.exists($a) && %!elems{$a} }
    method Bool { %!elems.Bool }
    method Numeric { %!elems.Numeric }
    method hash { %!elems.hash }
    method at_key($k) {
        Proxy.new(FETCH => { %!elems.exists($k) ?? True !! False },
                  STORE => -> $, $value { if $value { %!elems{$k} = True } else { %!elems.delete($k) }});
    }
    method exists_key($k) { self.exists($k) }
    method delete_key($k) { %!elems.delete($k) }

    # Constructor
    method new(*@args --> KeySet) {
        my %e;
        sub register-arg($arg) {
            given $arg {
                when Pair { %e{.key} = True; }
                when Set | KeySet { for .keys -> $key { %e{$key} = True; } }
                when Associative { for .pairs -> $p { register-arg($p); } }
                when Positional { for .list -> $p { register-arg($p); } }
                default { %e{$_} = True; }
            }
        }

        for @args {
            register-arg($_);
        }
        self.bless(*, :elems(%e));
    }

    submethod BUILD (:%!elems) { }

    submethod Str(Any:D $ : --> Str) { ~%!elems.keys }
    submethod gist(Any:D $ : --> Str) { "keyset({ %!elems.keys».gist.join(', ') })" }
    submethod perl(Any:D $ : --> Str) { 'KeySet.new(' ~ join(', ', map { .perl }, %!elems.keys) ~ ')' }

    method iterator() { %!elems.keys.iterator }
    method list() { %!elems.keys }
    method pick($count = 1) { %!elems.keys.pick($count) }
    method roll($count = 1) { %!elems.keys.roll($count) }
}
