my class Set does Setty {
    has Int $!total;
    has $!WHICH;
    has @!pairs;

    method total (--> Int) { $!total //= %!elems.elems }
    multi method WHICH (Set:D:) {
        $!WHICH := self.^name ~ '|' ~ %!elems.keys.sort if !$!WHICH.defined;
        $!WHICH
    }
    submethod BUILD (:%elems) {
        nqp::bindattr(self, Set, '%!elems', %elems);
        self;
    }

    multi method at_key(Set:D: \k --> Bool) {
        so %!elems.exists_key(k.WHICH);
    }

    method delete_key($k --> Bool) is hidden_from_backtrace {
        X::Immutable.new( method => 'delete_key', typename => self.^name ).throw;
    }
    method grab ($count?) {
        X::Immutable.new( method => 'grab', typename => self.^name ).throw;
    }
    method grabpairs ($count?) {
        X::Immutable.new( method => 'grabpairs', typename => self.^name ).throw;
    }

    multi method pairs(Set:D:) { # need to copy otherwise we can change the Set
        @!pairs ||= %!elems.values.map: { Enum.new(:key($_),:value(True)) };
    }

    method Set { self }
    method SetHash { SetHash.new(self.keys) }
}

# vim: ft=perl6 expandtab sw=4
