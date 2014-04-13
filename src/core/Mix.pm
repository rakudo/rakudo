my class Mix does Mixy {
    has Real $!total;
    has Real $!min;
    has Real $!max;

    method total (--> Real) { $!total //= [+] self.values }   
    method min   (--> Real) { $!min //= self.values.min }
    method max   (--> Real) { $!max //= self.values.max }
    method at_key($k --> Real) {
        my $key := $k.WHICH;
        %!elems.exists_key($key)
          ?? %!elems{$key}.value
          !! 0;
    }

    method delete ($a --> Real) {  # is DEPRECATED doesn't work in settings
        DEPRECATED("the :delete adverb");
        self.delete_key($a);
    }
    method delete_key($a --> Real) is hidden_from_backtrace {
        X::Immutable.new( method => 'delete_key', typename => self.^name ).throw;
    }
    method grab($count = 1 --> Real) is hidden_from_backtrace {
        X::Immutable.new( method => 'grab', typename => self.^name ).throw;
    }
    method grabpairs($count = 1 --> Real) is hidden_from_backtrace {
        X::Immutable.new( method => 'grabpairs', typename => self.^name ).throw;
    }

    method Mix { self }
    method MixHash { MixHash.new-fp(%!elems.values) }
    method Bag     {     Bag.new-fp(%!elems.values) }
    method BagHash { BagHash.new-fp(%!elems.values) }
}

# vim: ft=perl6 expandtab sw=4
