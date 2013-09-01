my role Baggy { Any }

only sub infix:<(.)>(**@p) {
    my $set = Set.new: @p.map(*.Set.keys);
    my @bags = @p.map(*.Bag);
    Bag.new-from-pairs($set.map({ ; $_ => [*] @bags>>.{$_} }));
}
# U+228D MULTISET MULTIPLICATION
only sub infix:<<"\x228D">>(|p) {
    infix:<(.)>(|p);
}

only sub infix:<(+)>(**@p) {
    my $set = Set.new: @p.map(*.Set.keys);
    my @bags = @p.map(*.Bag);
    Bag.new-from-pairs($set.map({ ; $_ => [+] @bags>>.{$_} }));
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

my class Bag is Iterable does Associative does Baggy {
    has %!elems; # should be UInt

    method default { 0 }
    method keys { %!elems.keys }
    method values { %!elems.values }
    method elems returns Int { [+] self.values }
    method exists($a) returns Bool { %!elems.exists($a) }
    method delete($a) is hidden_from_backtrace {
        X::Immutable.new( method => 'delete', typename => self.^name ).throw;
    }
    method Bool { %!elems.Bool }
    method Numeric { self.elems }
    method Real { self.elems }
    method hash { %!elems.hash }
    method Set { set self.keys }
    method KeySet { KeySet.new(self.keys) }
    method Bag { self }
    method KeyBag { KeyBag.new-from-pairs(self.hash) }

    method at_key($k) { +(%!elems{$k} // 0) }

    # Constructor
    method new(*@args --> Bag) {
        my %e;
        %e{$_}++ for @args;
        self.bless(:elems(%e));
    }
    method new-from-pairs(*@pairs --> Bag) {
        my %e;
        for @pairs {
            when Pair { %e{.key} = .value + (%e{.key} // 0); }
            %e{$_}++;
        }
        for %e -> $p {
            die "Negative values are not allowed in Bags" if $p.value < 0;
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

    multi method Str(Any:D $ : --> Str) { ~ self.pairs.map: { .key xx .value } }
    multi method gist(Any:D $ : --> Str) { "bag({ self.pairs>>.gist.join(', ') })" }
    multi method perl(Any:D $ : --> Str) {
        self.defined
          ?? '(' ~ %!elems.perl ~ ').Bag'
          !! "Bag";
   }

    method iterator() { %!elems.pairs.iterator }
    method list() { %!elems.keys }
    method pairs() { %!elems.pairs }

    method pick($count = 1) { self.KeyBag.pick($count) }
    method roll($count = 1) { self.KeyBag.roll($count) }
}

sub bag(*@a) returns Bag {
    Bag.new(|@a);
}
