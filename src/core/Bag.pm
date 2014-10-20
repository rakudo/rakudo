my class Bag does Baggy {
    has Int $!total;
    has $!WHICH;
    has @!pairs;  # should be Parcel, but .Parcel doesn't do what we need

    method total (--> Int) { $!total //= [+] self.values }

    multi method WHICH (Bag:D:) {
        $!WHICH //= self.^name
          ~ '|'
          ~ %!elems.keys.sort.map( { $_ ~ '(' ~ %!elems{$_}.value ~ ')' } );
    }
    submethod BUILD (:%!elems)  { }

    method at_key($k --> Int) {
        my $key := $k.WHICH;
        %!elems.exists_key($key)
          ?? %!elems{$key}.value
          !! 0;
    }
    method pairs() {
        @!pairs ||= %!elems.values.map: { Enum.new(:key(.key),:value(.value)) };
    }

    method delete_key($a) is hidden_from_backtrace {
        X::Immutable.new( method => 'delete_key', typename => self.^name ).throw;
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
