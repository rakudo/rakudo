my class Bag does Baggy {
    has Int $!total;
    has Int $!min;
    has Int $!max;
    has $!WHICH;

    method total (--> Int) { $!total //= [+] self.values }
    method min   (--> Int) { $!min //= self.values.min }
    method max   (--> Int) { $!max //= self.values.max }

    submethod WHICH { $!WHICH }
    submethod BUILD (:%elems)  {
        my @keys := %elems.keys.sort;
        $!WHICH  := self.^name
          ~ '|'
          ~ @keys.map( { $_ ~ '(' ~ %elems{$_}.value ~ ')' } );
        nqp::bindattr(self, Bag, '%!elems', %elems);
    }

    method at_key($k --> Int) {
        my $key := $k.WHICH;
        %!elems.exists_key($key)
          ?? %!elems{$key}.value
          !! 0;
    }

    method delete ($a --> Int) {  # is DEPRECATED doesn't work in settings
        DEPRECATED("the :delete adverb");
        self.delete_key($a);
    }
    method delete_key($a --> Int) is hidden_from_backtrace {
        X::Immutable.new( method => 'delete_key', typename => self.^name ).throw;
    }
    method grab($count = 1 --> Int) is hidden_from_backtrace {
        X::Immutable.new( method => 'grab', typename => self.^name ).throw;
    }
    method grabpairs($count = 1 --> Int) is hidden_from_backtrace {
        X::Immutable.new( method => 'grabpairs', typename => self.^name ).throw;
    }

    method Bag     { self }
    method BagHash { BagHash.new-fp(%!elems.values) }
    method Mix     {     Mix.new-fp(%!elems.values) }
    method MixHash { MixHash.new-fp(%!elems.values) }
}

# vim: ft=perl6 expandtab sw=4
