my class Bag does Baggy {
    has Int $!total;
    has $!WHICH;

#--- introspection methods
    multi method WHICH(Bag:D:)   {
        nqp::if(
          nqp::attrinited(self,Bag,'$!WHICH'),
          $!WHICH,
          $!WHICH := ObjAt.new('Bag!' ~ nqp::sha1(
            nqp::join('\0',Rakudo::Sorting.MERGESORT-str(
              Rakudo::QuantHash.BAGGY-RAW-KEY-VALUES(self)
            ))
          ))
        )
    }
    method total(Bag:D: --> Int:D) {
        nqp::if(
          nqp::attrinited(self,Bag,'$!total'),
          $!total,
          $!total := Rakudo::QuantHash.BAG-TOTAL(self.RAW-HASH)
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

    multi method DELETE-KEY(Bag:D: \k) {
        X::Immutable.new(method => 'DELETE-KEY', typename => self.^name).throw;
    }

#--- selection methods
    multi method grabpairs(Bag:D: $count?) {
        X::Immutable.new( method => 'grabpairs', typename => self.^name ).throw;
    }

#--- coercion methods
    multi method Bag(Bag:D:) { self }
    multi method BagHash(Bag:D) {
        nqp::if(
          (my $raw := self.RAW-HASH) && nqp::elems($raw),
          nqp::create(BagHash).SET-SELF(Rakudo::QuantHash.BAGGY-CLONE($raw)),
          nqp::create(BagHash)
        )
    }
    multi method Mix(Bag:D:) {
        nqp::if(
          (my $raw := self.RAW-HASH) && nqp::elems($raw),
          nqp::create(Mix).SET-SELF($raw),
          mix()
        )
    }
    multi method MixHash(Bag:D) {
        nqp::if(
          (my $raw := self.RAW-HASH) && nqp::elems($raw),
          nqp::create(MixHash).SET-SELF(Rakudo::QuantHash.BAGGY-CLONE($raw)),
          nqp::create(MixHash)
        )
    }
    method clone() {
        nqp::if(
          (my $raw := self.RAW-HASH) && nqp::elems($raw),
          nqp::clone(self),
          bag()
        )
    }

#--- illegal methods
    proto method classify-list(|) {
        X::Immutable.new(:method<classify-list>, :typename(self.^name)).throw;
    }
    proto method categorize-list(|) {
        X::Immutable.new(:method<categorize-list>, :typename(self.^name)).throw;
    }
}

# vim: ft=perl6 expandtab sw=4
