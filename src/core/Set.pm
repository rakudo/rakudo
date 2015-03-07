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

    multi method AT-KEY(Set:D: \k --> Bool) {
        %!elems.EXISTS-KEY(k.WHICH);
    }

    method DELETE-KEY($k --> Bool) is hidden_from_backtrace {
        X::Immutable.new( method => 'DELETE-KEY', typename => self.^name ).throw;
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
}

# vim: ft=perl6 expandtab sw=4
