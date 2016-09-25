my class Bag does Baggy {
    has Int $!total;
    has $!WHICH;

#--- introspection methods
    multi method WHICH(Bag:D:)   {
        nqp::if(
          nqp::attrinited(self,Bag,'$!WHICH'),
          $!WHICH,
          $!WHICH := self!WHICH
        )
    }
    method total(Bag:D: --> Int) {
        nqp::if(
          nqp::attrinited(self,Bag,'$!total'),
          $!total,
          $!total := self!TOTAL
        )
    }

#--- interface methods
    multi method DELETE-KEY(Bag:D: \k) {
        X::Immutable.new(method => 'DELETE-KEY', typename => self.^name).throw;
    }

#--- selection methods
    multi method grab(Bag:D: $count?) {
        X::Immutable.new( method => 'grab', typename => self.^name ).throw;
    }
    multi method grabpairs(Bag:D: $count?) {
        X::Immutable.new( method => 'grabpairs', typename => self.^name ).throw;
    }

#--- coercion methods
    method Bag     { self }
    method BagHash { BagHash.new-from-pairs(%!elems.values) }
    method Mix {
        nqp::p6bindattrinvres(nqp::create(Mix),Mix,'%!elems',%!elems)
    }
    method MixHash { MixHash.new-from-pairs(%!elems.values) }

    proto method classify-list(|) {
        X::Immutable.new(:method<classify-list>, :typename(self.^name)).throw;
    }
    proto method categorize-list(|) {
        X::Immutable.new(:method<categorize-list>, :typename(self.^name)).throw;
    }
}

# vim: ft=perl6 expandtab sw=4
