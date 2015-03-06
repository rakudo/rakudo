my class Mix does Mixy {
    has Real $!total;
    has $!WHICH;

    multi method WHICH (Mix:D:) {
        $!WHICH //= self.^name
          ~ '|'
          ~ %!elems.keys.sort.map( { $_ ~ '(' ~ %!elems{$_}.value ~ ')' } );
    }
    multi method pairs(Mix:D:) {    # copy values else we can change the Mix
        %!elems.values.map: { Enum.new(:key(.key),:value(.value)) };
    }
    multi method antipairs(Mix:D:) { # copy values else we can change the Mix
        %!elems.values.map: { Enum.new(:key(.value),:value(.key)) };
    }

    method total (--> Real) { $!total //= [+] self.values }
    multi method at_key(Mix:D: $k --> Real) {
        my $key := $k.WHICH;
        %!elems.exists_key($key)
          ?? %!elems{$key}.value
          !! 0;
    }

    method delete_key($a --> Real) is hidden_from_backtrace {
        X::Immutable.new( method => 'delete_key', typename => self.^name ).throw;
    }
    multi method grab($count? --> Real) is hidden_from_backtrace {
        X::Immutable.new( method => 'grab', typename => self.^name ).throw;
    }
    multi method grabpairs($count? --> Real) is hidden_from_backtrace {
        X::Immutable.new( method => 'grabpairs', typename => self.^name ).throw;
    }

    method Mix { self }
    method MixHash { MixHash.new-from-pairs(%!elems.values) }
    method Bag     {     Bag.new-from-pairs(%!elems.values) }
    method BagHash { BagHash.new-from-pairs(%!elems.values) }
}

# vim: ft=perl6 expandtab sw=4
