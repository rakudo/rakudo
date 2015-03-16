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

    multi method grab($count? --> Real) is hidden-from-backtrace {
        X::Immutable.new( method => 'grab', typename => self.^name ).throw;
    }
    multi method grabpairs($count? --> Real) is hidden-from-backtrace {
        X::Immutable.new( method => 'grabpairs', typename => self.^name ).throw;
    }

    method Mix { self }
    method MixHash { MixHash.new-from-pairs(%!elems.values) }
    method Bag     {     Bag.new-from-pairs(%!elems.values) }
    method BagHash { BagHash.new-from-pairs(%!elems.values) }

    # identical to Bag.pm
    multi method AT-KEY(Mix:D: \k) {
        my \v := %!elems.AT-KEY(k.WHICH);
        nqp::istype(v,Pair) ?? v.value !! 0;
    }
    multi method ASSIGN-KEY(Mix:D: \k,\v) is hidden-from-backtrace {
        fail X::Assignment::RO.new(typename => self.^name);
    }
    multi method DELETE-KEY(Mix:D: \k) is hidden-from-backtrace {
        fail X::Immutable.new(method => 'DELETE-KEY', typename => self.^name);
    }
}

# vim: ft=perl6 expandtab sw=4
