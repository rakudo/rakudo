my class Set is Iterable does Associative {
    has %!elems;

    method default { False }
    method keys { %!elems.keys }
    method values { %!elems.values }
    method elems returns Int { %!elems.elems }
    method exists($a) returns Bool { %!elems.exists($a) }
    method Bool { %!elems.Bool }
    method Numeric { %!elems.Numeric }
    method Real { %!elems.Numeric.Real }
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
    multi to-set ($thing) { X::Set::Coerce.new(:$thing).throw }

    multi method Str(Any:D $ : --> Str) { ~%!elems.keys() }
    multi method gist(Any:D $ : --> Str) { "set({ %!elems.keys».gist.join(', ') })" }
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

proto sub infix:<(elem)>($, $ --> Bool) {*}
multi sub infix:<(elem)>($a, Any $b --> Bool) { $a (elem) set($b) }
multi sub infix:<(elem)>($a, Set $b --> Bool) { $b.exists($a) }

proto sub infix:<(cont)>($, $ --> Bool) {*}
multi sub infix:<(cont)>(Any $a, $b --> Bool) { set($a) (cont) $b }
multi sub infix:<(cont)>(Set $a, $b --> Bool) { $a.exists($b) }

proto sub infix:<(|)>($a, $b) {*}
multi sub infix:<(|)>(Any $a, Any $b) { set($a) (|) set($b) }
multi sub infix:<(|)>(Set $a, Set $b) { Set.new: $a, $b }

proto sub infix:<(&)>($a, $b) {*}
multi sub infix:<(&)>(Any $a, Any $b) { set($a) (&) set($b) }
multi sub infix:<(&)>(Set $a, Set $b) { Set.new: $a.keys.grep: -> $k { ?$b{$k} } }

proto sub infix:<(-)>($, $ --> Set) {*}
multi sub infix:<(-)>(Any $a, Any $b --> Set) { set($a) (-) set($b) }
multi sub infix:<(-)>(Set $a, Set $b --> Set) { Set.new: $a.keys.grep: * !(elem) $b }

proto sub infix:<(^)>($, $ --> Set) {*}
multi sub infix:<(^)>(Any $a, Any $b --> Set) { set($a) (^) set($b) }
multi sub infix:<(^)>(Set $a, Set $b --> Set) { ($a (-) $b) (|) ($b (-) $a) }

proto sub infix:«(<=)»($, $ --> Bool) {*}
multi sub infix:«(<=)»(Any $a, Any $b --> Bool) { set($a) (<=) set($b) }
multi sub infix:«(<=)»(Set $a, Set $b --> Bool) { $a <= $b and so $a.keys.all (elem) $b }

proto sub infix:«(<)»($, $ --> Bool) {*}
multi sub infix:«(<)»(Any $a, Any $b --> Bool) { set($a) (<) set($b) }
multi sub infix:«(<)»(Set $a, Set $b --> Bool) { $a < $b and so $a.keys.all (elem) $b }

proto sub infix:«(>=)»($, $ --> Bool) {*}
multi sub infix:«(>=)»(Any $a, Any $b --> Bool) { set($a) (>=) set($b) }
multi sub infix:«(>=)»(Set $a, Set $b --> Bool) { $a >= $b and so $b.keys.all (elem) $a }

proto sub infix:«(>)»($, $ --> Bool) {*}
multi sub infix:«(>)»(Any $a, Any $b --> Bool) { set($a) (>) set($b) }
multi sub infix:«(>)»(Set $a, Set $b --> Bool) { $a > $b and so $b.keys.all (elem) $a }
