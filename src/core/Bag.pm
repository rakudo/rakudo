my class Bag does Baggy {
    has Int $!total;
    has $!WHICH;

    method BUILD(%!elems) { .freeze for %!elems.values; self }

    multi method WHICH (Bag:D:) {
        $!WHICH //= self.^name
          ~ '|'
          ~ %!elems.keys.sort.map( { $_ ~ '(' ~ %!elems{$_}.value ~ ')' } );
    }

    method total (--> Int) { $!total //= [+] self.values }

    multi method grab(Bag:D: $count?) {
        X::Immutable.new( method => 'grab', typename => self.^name ).throw;
    }
    multi method grabpairs(Bag:D: $count?) {
        X::Immutable.new( method => 'grabpairs', typename => self.^name ).throw;
    }

    method Bag     { self }
    method BagHash { BagHash.new-from-pairs(%!elems.values) }
    method Mix     {     Mix.new-from-pairs(%!elems.values) }
    method MixHash { MixHash.new-from-pairs(%!elems.values) }

    multi method AT-KEY(Bag:D: \k) {
        my $hash := nqp::getattr(%!elems,Map,'$!storage');
        my str $which = nqp::unbox_s(k.WHICH);
        nqp::existskey($hash,$which)
          ?? nqp::getattr(nqp::decont(nqp::atkey($hash,$which)),Pair,'$!value')
          !! 0
    }
    multi method ASSIGN-KEY(Bag:D: \k,\v) {
        X::Assignment::RO.new(typename => self.^name).throw;
    }
    multi method DELETE-KEY(Bag:D: \k) {
        X::Immutable.new(method => 'DELETE-KEY', typename => self.^name).throw;
    }
}

# vim: ft=perl6 expandtab sw=4
