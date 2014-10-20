my class Mix does Mixy {
    has Real $!total;
    has $!WHICH;
    has @!pairs;

    multi method WHICH (Mix:D:) {
        $!WHICH //= self.^name
          ~ '|'
          ~ %!elems.keys.sort.map( { $_ ~ '(' ~ %!elems{$_}.value ~ ')' } );
    }
    method pairs() {
        @!pairs ||= %!elems.values.map: { Enum.new(:key(.key),:value(.value)) };
    }

    method total (--> Real) { $!total //= [+] self.values }   
    method at_key($k --> Real) {
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
