my class Set does Setty {
    has Int $!total;
    has $!WHICH;

    method ISINSET(\key) { True }

    multi method WHICH (Set:D:) {
        nqp::if(
          nqp::attrinited(self,Set,'$!WHICH'),
          $!WHICH,
          $!WHICH := self.^name ~ '|' ~ %!elems.keys.sort
        )
    }
    method total (--> Int) {
        nqp::if(
          nqp::attrinited(self,Set,'$!total'),
          $!total,
          $!total = %!elems.elems
        )
    }

    method grab ($count?) {
        X::Immutable.new( method => 'grab', typename => self.^name ).throw;
    }
    method grabpairs ($count?) {
        X::Immutable.new( method => 'grabpairs', typename => self.^name ).throw;
    }

    multi method pairs(Set:D:) {    # must copy else we can change the Set
        %!elems.values.quickmap: { Pair.new($_,True) };
    }
    multi method antipairs(Set:D:) { # must copy else we can change the Set
        %!elems.values.quickmap: { Pair.new(True,nqp::decont($_)) };
    }

    method Set { self }
    method SetHash { SetHash.new(self.keys) }

    multi method AT-KEY(Set:D: \k --> Bool) {
        %!elems.EXISTS-KEY(k.WHICH);
    }
    multi method ASSIGN-KEY(Set:D: \k,\v) {
        X::Assignment::RO.new(typename => self.^name).throw;
    }
    multi method DELETE-KEY(Set:D: \k) {
        X::Immutable.new(method => 'DELETE-KEY', typename => self.^name).throw;
    }
}

# vim: ft=perl6 expandtab sw=4
