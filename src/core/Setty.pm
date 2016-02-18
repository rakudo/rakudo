my role Setty does QuantHash {
    has %!elems; # key.WHICH => key

    submethod BUILD(:%!elems --> Nil)  { }
    method default(--> Bool) { False }

    multi method keys(Setty:D:) {
        %!elems.values
    }
    multi method kv(Setty:D:) {
        %!elems.values.map: -> \key { |(key,self.ISINSET(key.WHICH)) }
    }
    multi method values(Setty:D:) {
        %!elems.values.map: -> \key { self.ISINSET(key.WHICH) }
    }
    multi method pairs(Setty:D:) {
        %!elems.values.map: -> \key { Pair.new(key,self.ISINSET(key.WHICH)) }
    }
    multi method antipairs(Setty:D:) {
        %!elems.values.map: -> \key { Pair.new(True,key) }
    }

    method elems(Setty:D: --> Int) { %!elems.elems }
    method total(Setty:D: --> Int) { %!elems.elems }
    method minpairs() { self.pairs }
    method maxpairs() { self.pairs }
    multi method Bool(Setty:D:) { %!elems.Bool }

    multi method hash(Setty:D: --> Hash) {
        my \e = Hash.^parameterize(Bool, Any).new;
        e{$_} = True for %!elems.values;
        e;
    }

    multi method new(Setty: +@args) {
        my %e;
        %e{$_.WHICH} = $_ for @args;
        self.bless(:elems(%e))
    }
    method new-from-pairs(*@pairs --> Setty) {
        my %e;
        for @pairs {
            when Pair {
                %e{.key.WHICH} //= $_.key if .value;
            }
            default {
                %e{.WHICH} //= $_;
            }
        }
        self.bless(:elems(%e));
    }

    multi method ACCEPTS(Setty:U: $other) {
        $other.^does(self)
    }
    multi method ACCEPTS(Setty:D: Seq:D \seq) {
        self.ACCEPTS(seq.list)
    }
    multi method ACCEPTS(Setty:D: $other) {
        $other (<=) self && self (<=) $other
    }

    multi method Str(Setty:D $ : --> Str) { ~ %!elems.values }
    multi method gist(Setty:D $ : --> Str) {
        my $name := self.^name;
        ( $name eq 'Set' ?? 'set' !! "$name.new" )
        ~ '('
        ~ %!elems.values.map( {.gist} ).join(', ')
        ~ ')';
    }
    multi method perl(Setty:D $ : --> Str) {
        my $name := self.^name;
        ( $name eq 'Set' ?? 'set' !! "$name.new" )
        ~ '('
        ~ %!elems.values.map( {.perl} ).join(',')
        ~ ')';
    }

    method grab(Setty:D: $count = 1) {
        (%!elems{ %!elems.keys.pick($count) }:delete).cache;
    }
    method grabpairs(Setty:D: $count = 1) {
        (%!elems{ %!elems.keys.pick($count) }:delete).map( { ($_=>True) } );
    }

    proto method pick(|) { * }
    multi method pick(Setty:D:)       { %!elems.values.pick()       }
    multi method pick(Setty:D: $count) { %!elems.values.pick($count) }

    proto method roll(|) { * }
    multi method roll(Setty:D:)       { %!elems.values.roll()       }
    multi method roll(Setty:D: $count) { %!elems.values.roll($count) }

    multi method EXISTS-KEY(Setty:D: \k --> Bool) {
        nqp::p6bool(
          %!elems.elems && nqp::existskey(%!elems, nqp::unbox_s(k.WHICH))
        );
    }

    method Bag { Bag.new( %!elems.values ) }
    method BagHash { BagHash.new( %!elems.values ) }

    # TODO: WHICH will require the capability for >1 pointer in ObjAt
}

# vim: ft=perl6 expandtab sw=4
