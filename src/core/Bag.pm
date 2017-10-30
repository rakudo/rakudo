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
          $!total := Rakudo::QuantHash.BAG-TOTAL($!elems)
        )
    }

#--- interface methods
    method STORE(*@pairs, :$initialize --> Bag:D) {
        nqp::if(
          (my $iterator := @pairs.iterator).is-lazy,
          Failure.new(X::Cannot::Lazy.new(:action<initialize>,:what(self.^name))),
          nqp::if(
            $initialize,
            self.SET-SELF(
              Rakudo::QuantHash.ADD-PAIRS-TO-BAG(
                nqp::create(Rakudo::Internals::IterationSet), $iterator
              )
            ),
            X::Assignment::RO.new(value => self).throw
          )
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
          $!elems && nqp::elems($!elems),
          nqp::create(BagHash).SET-SELF(Rakudo::QuantHash.BAGGY-CLONE($!elems)),
          nqp::create(BagHash)
        )
    }
    multi method Mix(Bag:D:) {
        nqp::if(
          $!elems && nqp::elems($!elems),
          nqp::create(Mix).SET-SELF($!elems),
          mix()
        )
    }
    multi method MixHash(Bag:D) {
        nqp::if(
          $!elems && nqp::elems($!elems),
          nqp::create(MixHash).SET-SELF(Rakudo::QuantHash.BAGGY-CLONE($!elems)),
          nqp::create(MixHash)
        )
    }
    method clone() {
        nqp::if(
          $!elems && nqp::elems($!elems),
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
