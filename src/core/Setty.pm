my role Setty does QuantHash {
    has %!elems; # key.WHICH => key

    method BUILD (:%!elems) {}
    method default(--> Bool) { False }
    method keys { %!elems.values }
    method values { True xx %!elems.elems }
    method elems(--> Int) { %!elems.elems }
    method total(--> Int) { %!elems.elems }
    method exists ($k --> Bool) {  # is DEPRECATED doesn't work in settings
        once DEPRECATED("Method 'Setty.exists'","the :exists adverb");
        self.exists_key($k);
    }
    method exists_key($k --> Bool) {
        so nqp::existskey(%!elems, nqp::unbox_s($k.WHICH));
    }
    method Bool { %!elems.Bool }

    method hash(--> Hash) {
        my %e;
        %e{$_} = True for %!elems.values;
        %e;
    }

    method new(*@args --> Setty) {
        my %e;
        %e{$_.WHICH} = $_ for @args;
        self.bless(:elems(%e));
    }
    method new-fp(*@pairs --> Setty) {
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
    method pairs() { %!elems.values.map({ $_ => True }) } 
    method grab($count = 1) {
        (%!elems{ %!elems.keys.pick($count) }:delete).list;
    }
    method pick($count = 1) { %!elems.values.pick($count) }
    method roll($count = 1) { %!elems.values.roll($count) }

    # TODO: WHICH will require the capability for >1 pointer in ObjAt
}
