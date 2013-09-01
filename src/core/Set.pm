# U+2205 EMPTY SET
#constant term:<<"\x2205">> = set();  # invoke() not implemented in class 'QAST::Want'

proto sub infix:<(elem)>($, $ --> Bool) {*}
multi sub infix:<(elem)>($a, Any $b --> Bool) {
    $a (elem) $b.Set;
}
multi sub infix:<(elem)>($a, Set $b --> Bool) {
    $b.exists($a);
}
# U+2208 ELEMENT OF
only sub infix:<<"\x2208">>($a, $b --> Bool) {
    $a (elem) $b;
}
# U+2209 NOT AN ELEMENT OF
only sub infix:<<"\x2209">>($a, $b --> Bool) {
    $a !(elem) $b;
}

proto sub infix:<(cont)>($, $ --> Bool) {*}
multi sub infix:<(cont)>(Any $a, $b --> Bool) {
    $a.Set (cont) $b;
}
multi sub infix:<(cont)>(Set $a, $b --> Bool) {
    $a.exists($b);
}
# U+220B CONTAINS AS MEMBER
only sub infix:<<"\x220B">>($a, $b --> Bool) {
    $a (cont) $b;
}
# U+220C DOES NOT CONTAIN AS MEMBER
only sub infix:<<"\x220C">>($a, $b --> Bool) {
    $a !(cont) $b;
}

only sub infix:<(|)>(**@p) {
    my $set = Set.new: @p.map(*.Set.keys);
    if @p.grep(Baggy) {
        my @bags = @p.map(*.Bag);
        Bag.new-from-pairs($set.map({ ; $_ => [max] @bags>>.{$_} }));
    }
    else {
        $set;
    }
}
# U+222A UNION
only sub infix:<<"\x222A">>(|p) {
    infix:<(|)>(|p);
}

only sub infix:<(&)>(**@p) {
    my $base_set = @p ?? @p[0].Set !! set();
    if @p.grep(Baggy) {
        my @bags = @p.map(*.Bag);
        Bag.new-from-pairs($base_set.map({ ; $_ => [min] @bags>>.{$_} }));
    }
    else {
        my @sets = @p.map(*.Set);
        Set.new($base_set.grep: -> $k { @sets>>.{$k}.all });
    }
}
# U+2229 INTERSECTION
only sub infix:<<"\x2229">>(|p) {
    infix:<(&)>(|p);
}

only sub infix:<(-)>(**@p) {
    return set() unless @p;
    if @p[0] ~~ Baggy {
        my @bags = @p.map(*.Bag);
        my $base = @bags.shift;
        Bag.new-from-pairs($base.keys.map({ ; $_ => $base{$_} - [+] @bags>>.{$_} }));
    }
    else {
        my @sets = @p.map(*.Set);
        my $base = @sets.shift;
        Set.new: $base.keys.grep(* !(elem) @sets.any );
    }
}
# U+2216 SET MINUS
only sub infix:<<"\x2216">>($a, $b) {
    $a (-) $b;
}

proto sub infix:<(^)>($, $ --> Set) {*}
multi sub infix:<(^)>(Any $a, Any $b --> Set) {
    $a.Set (^) $b.Set;
}
multi sub infix:<(^)>(Set $a, Set $b --> Set) {
    ($a (-) $b) (|) ($b (-) $a);
}
# U+2296 CIRCLED MINUS
only sub infix:<<"\x2296">>($a, $b --> Set) {
    $a (^) $b;
}

# TODO: polymorphic eqv
# multi sub infix:<eqv>(Any $a, Any $b --> Bool) {
#     $a.Set eqv $b.Set
# }
# multi sub infix:<eqv>(Set $a, Set $b --> Bool) {
#     $a == $b and so $a.keys.all (elem) $b
# }

proto sub infix:<<(<=)>>($, $ --> Bool) {*}
multi sub infix:<<(<=)>>(Any $a, Any $b --> Bool) {
    $a.Set (<=) $b.Set;
}
multi sub infix:<<(<=)>>(Set $a, Set $b --> Bool) {
    $a <= $b and so $a.keys.all (elem) $b
}
# U+2286 SUBSET OF OR EQUAL TO
only sub infix:<<"\x2286">>($a, $b --> Bool) {
    $a (<=) $b;
}
# U+2288 NEITHER A SUBSET OF NOR EQUAL TO
only sub infix:<<"\x2288">>($a, $b --> Bool) {
    $a !(<=) $b;
}

proto sub infix:<<(<)>>($, $ --> Bool) {*}
multi sub infix:<<(<)>>(Any $a, Any $b --> Bool) {
    $a.Set (<) $b.Set;
}
multi sub infix:<<(<)>>(Set $a, Set $b --> Bool) {
    $a < $b and so $a.keys.all (elem) $b;
}
# U+2282 SUBSET OF
only sub infix:<<"\x2282">>($a, $b --> Bool) {
    $a (<) $b;
}
# U+2284 NOT A SUBSET OF
only sub infix:<<"\x2284">>($a, $b --> Bool) {
    $a !(<) $b;
}

proto sub infix:<<(>=)>>($, $ --> Bool) {*}
multi sub infix:<<(>=)>>(Any $a, Any $b --> Bool) {
    $a.Set (>=) $b.Set
}
multi sub infix:<<(>=)>>(Set $a, Set $b --> Bool) {
    $a >= $b and so $b.keys.all (elem) $a;
}
# U+2287 SUPERSET OF OR EQUAL TO
only sub infix:<<"\x2287">>($a, $b --> Bool) {
    $a (>=) $b;
}
# U+2289 NEITHER A SUPERSET OF NOR EQUAL TO
only sub infix:<<"\x2289">>($a, $b --> Bool) {
    $a !(>=) $b;
}

proto sub infix:<<(>)>>($, $ --> Bool) {*}
multi sub infix:<<(>)>>(Any $a, Any $b --> Bool) {
    $a.Set (>) $b.Set;
}
multi sub infix:<<(>)>>(Set $a, Set $b --> Bool) {
    $a > $b and so $b.keys.all (elem) $a;
}
# U+2283 SUPERSET OF
only sub infix:<<"\x2283">>($a, $b --> Bool) {
    $a (>) $b;
}
# U+2285 NOT A SUPERSET OF
only sub infix:<<"\x2285">>($a, $b --> Bool) {
    $a !(>) $b;
}

my class Set is Iterable does Associative {
    has %!elems;

    method default { False }
    method keys { %!elems.keys }
    method values { %!elems.values }
    method elems returns Int { %!elems.elems }
    method exists($a) returns Bool { %!elems.exists($a) }
    method delete($a) is hidden_from_backtrace {
        X::Immutable.new( method => 'delete', typename => self.^name ).throw;
    }
    method Bool { %!elems.Bool }
    method Numeric { %!elems.Numeric }
    method Real { %!elems.Numeric.Real }
    method hash { %!elems.hash }
    method Set { self }
    method KeySet { KeySet.new(self.keys) }
    method Bag { bag self.keys }
    method KeyBag { KeyBag.new(self.keys) }

    method at_key($k) { ?(%!elems{$k} // False) }

    # Constructor
    method new(*@args --> Set) {
        my %e;
        %e{$_} = True for @args;
        self.bless(:elems(%e));
    }

    submethod BUILD (:%!elems) { }

    method ACCEPTS($other) {
        self.defined
          ?? $other (<=) self && self (<=) $other
          !! $other.^does(self);
    }

    multi method Str(Any:D $ : --> Str) { ~%!elems.keys() }
    multi method gist(Any:D $ : --> Str) { "set({ %!elems.keys».gist.join(', ') })" }
    multi method perl(Any:D $ : --> Str) { 'set(' ~ join(', ', map { .perl }, %!elems.keys) ~ ')' }

    method iterator() { %!elems.keys.iterator }
    method list() { %!elems.keys }
    method pick($count = 1) { %!elems.keys.pick($count) }
    method roll($count = 1) { %!elems.keys.roll($count) }

    # TODO: WHICH will require the capability for >1 pointer in ObjAt
}

sub set(*@args --> Set) {
    Set.new(@args);
}
