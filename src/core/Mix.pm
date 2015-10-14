my class Mix does Mixy {
    has $!WHICH;
    has Real $!total;

#--- interface methods
    multi method DELETE-KEY(Mix:D: \k) {
        X::Immutable.new(method => 'DELETE-KEY', typename => self.^name).throw;
    }

#--- introspection methods
    multi method WHICH(Mix:D:)    { $!WHICH //= self!WHICH }
    method total(Mix:D: --> Real) { $!total //= self!TOTAL }

#--- selection methods
    multi method grab($count? --> Real) {
        X::Immutable.new( method => 'grab', typename => self.^name ).throw;
    }
    multi method grabpairs($count? --> Real) {
        X::Immutable.new( method => 'grabpairs', typename => self.^name ).throw;
    }

#--- coercion methods
    method Mix { self }
    method MixHash { MixHash.new-from-pairs(%!elems.values) }
    method Bag     {     Bag.new-from-pairs(%!elems.values) }
    method BagHash { BagHash.new-from-pairs(%!elems.values) }
}

# vim: ft=perl6 expandtab sw=4
