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
    method total(Bag:D: --> Int:D) {
        nqp::if(
          nqp::attrinited(self,Bag,'$!total'),
          $!total,
          $!total := Rakudo::QuantHash.BAG-TOTAL(self.raw_hash)
        )
    }

#--- object creation methods
    multi method new(Bag:_:) {
        nqp::if(
          nqp::eqaddr(self.WHAT,Bag),
          bag(),
          nqp::create(self)
        )
    }

#--- interface methods
    method SET-SELF(Bag:D: \elems) {
        nqp::if(
          nqp::elems(elems),
          nqp::stmts(                 # need to have allocated %!elems
            nqp::bindattr(%!elems,Map,'$!storage',elems),
            self
          ),
          bag()
        )
    }

    my $p0 := nqp::p6bindattrinvres(nqp::create(Pair),Pair,'$!value',0);
    multi method AT-KEY(Bag:D: \k) {
        nqp::getattr(
          nqp::ifnull(nqp::atkey(self.raw_hash,k.WHICH),$p0),
          Pair,
          '$!value'
        )
    }
    multi method DELETE-KEY(Bag:D: \k) {
        X::Immutable.new(method => 'DELETE-KEY', typename => self.^name).throw;
    }

#--- selection methods
    multi method grabpairs(Bag:D: $count?) {
        X::Immutable.new( method => 'grabpairs', typename => self.^name ).throw;
    }

#--- coercion methods
    multi method Bag(Bag:D:) { self }
    multi method Mix(Bag:D:) {
        nqp::p6bindattrinvres(nqp::create(Mix),Mix,'%!elems',%!elems)
    }

    method clone() { nqp::clone(self) }

    proto method classify-list(|) {
        X::Immutable.new(:method<classify-list>, :typename(self.^name)).throw;
    }
    proto method categorize-list(|) {
        X::Immutable.new(:method<categorize-list>, :typename(self.^name)).throw;
    }
}

# vim: ft=perl6 expandtab sw=4
