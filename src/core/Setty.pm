my role Setty does QuantHash {
    has %!elems; # key.WHICH => key

    method BUILD (:%!elems)  { self }
    method default(--> Bool) { False }

    multi method keys(Setty:D:)   { %!elems.values }
    multi method kv(Setty:D:)     { %!elems.values X, True }
    multi method values(Setty:D:) { True xx %!elems.elems }

    method elems(--> Int) { %!elems.elems }
    method total(--> Int) { %!elems.elems }
    method minpairs(--> List) { self.pairs }
    method maxpairs(--> List) { self.pairs }
    multi method exists_key(Setty:D: $k --> Bool) {
        so ( %!elems && nqp::existskey(%!elems, nqp::unbox_s($k.WHICH)) );
    }
    multi method Bool(Setty:D:) { %!elems.Bool }

    multi method hash(Setty:D: --> Hash) {
        my %e;
        %e{$_} = True for %!elems.values;
        %e;
    }

    method new(*@args --> Setty) {
        my %e;
        %e{$_.WHICH} = $_ for @args;
        nqp::create(self).BUILD(:elems(%e));
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

    method ACCEPTS($other) {
        self.defined
          ?? $other (<=) self && self (<=) $other
          !! $other.^does(self);
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

    method list() { %!elems.values }
    multi method pairs(Setty:D:)    { %!elems.values.map: { $_ => True } }
    multi method antipairs(Setty:D:) { %!elems.values.map: { True => $_ } }
    method grab($count = 1) {
        (%!elems{ %!elems.keys.pick($count) }:delete).list;
    }
    method grabpairs($count = 1) {
        (%!elems{ %!elems.keys.pick($count) }:delete).map( { ($_=>True) } );
    }

    proto method pick(|) { * }
    multi method pick()       { %!elems.values.pick()       }
    multi method pick($count) { %!elems.values.pick($count) }

    proto method roll(|) { * }
    multi method roll()       { %!elems.values.roll()       }
    multi method roll($count) { %!elems.values.roll($count) }

    # TODO: WHICH will require the capability for >1 pointer in ObjAt
}

# vim: ft=perl6 expandtab sw=4
