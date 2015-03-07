my class Bag does Baggy {
    has Int $!total;
    has $!WHICH;

    method total (--> Int) { $!total //= [+] self.values }

    multi method WHICH (Bag:D:) {
        $!WHICH //= self.^name
          ~ '|'
          ~ %!elems.keys.sort.map( { $_ ~ '(' ~ %!elems{$_}.value ~ ')' } );
    }

    multi method AT-KEY(Bag:D: $k --> Int) {
        my $key := $k.WHICH;
        %!elems.EXISTS-KEY($key)
          ?? %!elems{$key}.value
          !! 0;
    }
    multi method pairs(Bag:D:) {    # must copy, else we would change the Bag
        %!elems.values.map: { Enum.new(:key(.key),:value(.value)) };
    }
    multi method antipairs(Bag:D:) { # must copy, else we would change the Bag
        %!elems.values.map: { Enum.new(:key(.value),:value(.key)) };
    }

    method DELETE-KEY($a) is hidden_from_backtrace {
        X::Immutable.new( method => 'DELETE-KEY', typename => self.^name ).throw;
    }
    multi method grab(Bag:D: $count?) is hidden_from_backtrace {
        X::Immutable.new( method => 'grab', typename => self.^name ).throw;
    }
    multi method grabpairs(Bag:D: $count?) is hidden_from_backtrace {
        X::Immutable.new( method => 'grabpairs', typename => self.^name ).throw;
    }

    method Bag     { self }
    method BagHash { BagHash.new-from-pairs(%!elems.values) }
    method Mix     {     Mix.new-from-pairs(%!elems.values) }
    method MixHash { MixHash.new-from-pairs(%!elems.values) }
}

# vim: ft=perl6 expandtab sw=4
