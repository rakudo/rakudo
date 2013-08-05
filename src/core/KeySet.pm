my class KeySet is Iterable does Associative {
    has %!elems;

    method default { False }
    method keys { %!elems.keys }
    method values { %!elems.values }
    method elems returns Int { %!elems.elems }
    method exists($a) returns Bool { %!elems.exists($a) && %!elems{$a} }
    method Bool { %!elems.Bool }
    method Numeric { %!elems.Numeric }
    method Real { %!elems.Numeric.Real }
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

    multi method Str(Any:D $ : --> Str) { ~%!elems.keys }
    multi method gist(Any:D $ : --> Str) { "keyset({ %!elems.keys».gist.join(', ') })" }
    multi method perl(Any:D $ : --> Str) { 'KeySet.new(' ~ join(', ', map { .perl }, %!elems.keys) ~ ')' }

    method iterator() { %!elems.keys.iterator }
    method list() { %!elems.keys }
    method pick($count = 1) { %!elems.keys.pick($count) }
    method roll($count = 1) { %!elems.keys.roll($count) }
}
