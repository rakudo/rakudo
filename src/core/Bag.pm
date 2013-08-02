my role Baggy { Any }

my class Bag is Iterable does Associative does Baggy {
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

multi sub infix:<(|)>(Baggy $a, Any $b --> Bag) { $a (|) bag($b) }
multi sub infix:<(|)>(Any $a, Baggy $b --> Bag) { bag($a) (|) $b }
multi sub infix:<(|)>(Baggy $a, Baggy $b --> Bag) { bag((set($a) (|) set($b)).map({ ; $_ => $a{$_} max $b{$_} })) }

multi sub infix:<(&)>(Baggy $a, Any $b --> Bag) { $a (&) bag($b) }
multi sub infix:<(&)>(Any $a, Baggy $b --> Bag) { bag($a) (&) $b }
multi sub infix:<(&)>(Baggy $a, Baggy $b --> Bag) { bag((set($a) (&) set($b)).map({ ; $_ => $a{$_} min $b{$_} })) }

proto sub infix:<(.)>($, $ --> Bag) {*}
multi sub infix:<(.)>(Any $a, Any $b --> Bag) { bag($a) (.) bag($b) }
multi sub infix:<(.)>(Bag $a, Bag $b --> Bag) { bag((set($a) (|) set($b)).map({ ; $_ => $a{$_} * $b{$_} })) }

proto sub infix:<(+)>($, $ --> Bag) {*}
multi sub infix:<(+)>(Any $a, Any $b --> Bag) { bag($a) (+) bag($b) }
multi sub infix:<(+)>(Bag $a, Bag $b --> Bag) { bag((set($a) (|) set($b)).map({ ; $_ => $a{$_} + $b{$_} })) }
