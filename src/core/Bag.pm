my class Bag does Baggy {
    has Int $!total;
    has $!WHICH;

    method total {
        $!total //=
          [+] nqp::getattr(self, Bag, '%!elems').values.map( { .value } );
    }
    submethod WHICH { $!WHICH }
    submethod BUILD (:%elems)  {
        my @keys := %elems.keys.sort;
        $!WHICH  := self.^name
          ~ '|'
          ~ @keys.map( { $_ ~ '(' ~ %elems{$_}.value ~ ')' } );
        nqp::bindattr(self, Bag, '%!elems', %elems);
    }

    method at_key($k --> Int) {
        my $elems := nqp::getattr(self, Bag, '%!elems');
        my $key   := $k.WHICH;
        $elems.exists_key($key)
          ?? $elems{$key}.value
          !! 0;
    }

    method delete ($a --> Int) {  # is DEPRECATED doesn't work in settings
        once DEPRECATED("Method 'Bag.delete'","the :delete adverb");
        self.delete_key($a);
    }
    method delete_key($a --> Int) is hidden_from_backtrace {
        X::Immutable.new( method => 'delete_key', typename => self.^name ).throw;
    }
    method grab($count = 1 --> Int) is hidden_from_backtrace {
        X::Immutable.new( method => 'grab', typename => self.^name ).throw;
    }

    method Bag     { self }
    method BagHash { BagHash.new-fp(nqp::getattr(self, Bag, '%!elems').values) }
    method Mix     {     Mix.new-fp(nqp::getattr(self, Bag, '%!elems').values) }
    method MixHash { MixHash.new-fp(nqp::getattr(self, Bag, '%!elems').values) }
}
