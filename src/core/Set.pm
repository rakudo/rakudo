my class Set does Setty {
    has Int $!total;
    has $!WHICH;

    method total (--> Int) { $!total //= %!elems.elems }
    multi method WHICH (Set:D:) {
        $!WHICH := self.^name ~ '|' ~ %!elems.keys.sort if !$!WHICH.defined;
        $!WHICH
    }
    submethod BUILD (:%elems) {
        nqp::bindattr(self, Set, '%!elems', %elems);
        self;
    }

    method grab ($count?) {
        X::Immutable.new( method => 'grab', typename => self.^name ).throw;
    }
    method grabpairs ($count?) {
        X::Immutable.new( method => 'grabpairs', typename => self.^name ).throw;
    }

    multi method pairs(Set:D:) {    # must copy else we can change the Set
        %!elems.values.map: { Enum.new(:key($_),:value(True)) };
    }
    multi method antipairs(Set:D:) { # must copy else we can change the Set
        %!elems.values.map: { Enum.new(:key(True),:value($_)) };
    }

    method Set { self }
    method SetHash { SetHash.new(self.keys) }

    multi method AT-KEY(Set:D: \k --> Bool) {
        %!elems.EXISTS-KEY(k.WHICH);
    }
    multi method ASSIGN-KEY(Set:D: \k,\v) is hidden_from_backtrace {
        fail X::Assignment::RO.new(typename => self.^name);
    }
    multi method DELETE-KEY(Set:D: \k) is hidden_from_backtrace {
        fail X::Immutable.new(method => 'DELETE-KEY', typename => self.^name);
    }
}

# vim: ft=perl6 expandtab sw=4
