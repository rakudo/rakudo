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

    method default(--> Int) { 0 }
    method keys { %!elems.values.map( {.key} ) }
    method values { %!elems.values.map( {.value} ) }
    method elems(--> Int) { [+] self.values }
    method exists($k --> Bool) { %!elems.exists($k.WHICH) }
    method delete($a --> Int) is hidden_from_backtrace {
        X::Immutable.new( method => 'delete', typename => self.^name ).throw;
    }
    method Bool { %!elems.Bool }
    method Numeric { self.elems }
    method Real { self.elems }
    method Set { set self.keys }
    method KeySet { KeySet.new(self.keys) }
    method Bag { self }
    method KeyBag { KeyBag.new-from-pairs(%!elems.values) }

    method at_key($k --> Int) {
        my $key := $k.WHICH;
        nqp::existskey(%!elems, nqp::unbox_s($key))
          ?? %!elems{$key}.value
          !! 0;
    }

    method hash(--> Hash) { %!elems.values.hash }

    method new(*@args --> Bag) {
        my %e;
        # need explicit signature because of #119609
        -> $_ { (%e{$_.WHICH} //= ($_ => 0)).value++ } for @args;
        self.bless(:elems(%e));
    }
    method new-from-pairs(*@pairs --> Bag) {
        my %e;
        for @pairs {
            when Pair {
                (%e{$_.key.WHICH} //= ($_ => 0)).value += $_.value;
            }
            default {
                (%e{$_.WHICH} //= ($_ => 0)).value++;
            }
        }
        my @toolow;
        for %e -> $p {
            my $pair := $p.value;
            @toolow.push( $pair.key ) if $pair.value <  0;
            %e.delete($p.key)         if $pair.value <= 0;
        }
        die "Found negative values for {@toolow} in {self.^name}" if @toolow;
        self.bless(:elems(%e));
    }

    submethod BUILD (:%!elems) { }

    method ACCEPTS($other) {
        self.defined
          ?? $other (<+) self && self (<+) $other
          !! $other.^does(self);
    }

    multi method Str(Bag:D $ : --> Str) { ~ self.pairs.map({ .key xx .value }) }
    multi method gist(Bag:D $ : --> Str) {
        my $name := self.^name;
        ( $name eq 'Bag' ?? 'bag' !! "$name.new" )
        ~ '('
        ~ %!elems.values.map( {
            .value > 1  # rather arbitrarily
              ?? "{.key.gist} xx {.value}"
              !! .key.gist xx .value
        } ).join(', ')
        ~ ')';
    }
    multi method perl(Bag:D $ : --> Str) {
        my $name := self.^name;
        ( $name eq 'Bag' ?? 'bag' !! "$name.new" )
        ~ '('
        ~ %!elems.values.map( {
            .value > 1  # rather arbitrarily
              ?? "{.key.perl} xx {.value}"
              !! .key.perl xx .value
        } ).join(',')
        ~ ')';
    }

    method iterator() { %!elems.values.iterator }
    method list() { self.keys }
    method pairs() { %!elems.values }

    method pick($count = 1) {
        %!elems.values.map({ .key xx .value }).pick($count);
    }
    method roll($count = 1) {
        %!elems.values.map({ .key xx .value }).roll($count);
    }
}

sub bag(*@a) returns Bag {
    Bag.new(|@a);
}
