my class Bag does Baggy {
    has ValueObjAt $!WHICH;
    has Int        $!total;

    method ^parameterize(Mu \base, Mu \type) {
        Rakudo::Internals.PARAMETERIZE-KEYOF(base,type)
    }

#--- introspection methods
    multi method WHICH(Bag:D: --> ValueObjAt:D)   {
        nqp::isconcrete($!WHICH) ?? $!WHICH !! self!WHICH
    }

    method !WHICH() {
        $!WHICH := nqp::box_s(
          nqp::concat(
            nqp::if(
              nqp::eqaddr(self.WHAT,Bag),
              'Bag|',
              nqp::concat(nqp::unbox_s(self.^name), '|')
            ),
            nqp::sha1(
              nqp::join('\0',Rakudo::Sorting.MERGESORT-str(
                Rakudo::QuantHash.BAGGY-RAW-KEY-VALUES(self)
              ))
            )
          ),
          ValueObjAt
        )
    }
    method total(Bag:D: --> Int:D) {
        nqp::attrinited(self,Bag,'$!total')
          ?? $!total
          !! ($!total := Rakudo::QuantHash.BAG-TOTAL($!elems))
    }


#--- interface methods
    multi method STORE(Bag:D: Iterable:D \iterable, :INITIALIZE($)! --> Bag:D) {
        (my \iterator := iterable.iterator).is-lazy
          ?? self.fail-iterator-cannot-be-lazy('initialize')
          !! self.SET-SELF(Rakudo::QuantHash.ADD-PAIRS-TO-BAG(
               nqp::create(Rakudo::Internals::IterationSet),iterator,self.keyof
             ))
    }
    multi method STORE(Bag:D: \objects, \values, :INITIALIZE($)! --> Bag:D) {
        self.SET-SELF(
          Rakudo::QuantHash.ADD-OBJECTS-VALUES-TO-BAG(
            nqp::create(Rakudo::Internals::IterationSet),
            objects.iterator,
            values.iterator,
            self.keyof
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
    multi method BagHash(Bag:D:) {
        $!elems && nqp::elems($!elems)
          ?? nqp::create(BagHash).SET-SELF(
               Rakudo::QuantHash.BAGGY-CLONE($!elems))
          !! nqp::create(BagHash)
    }
    multi method Mix(Bag:D:) {
        $!elems && nqp::elems($!elems)
          ?? nqp::create(Mix).SET-SELF($!elems)
          !! mix()
    }
    multi method MixHash(Bag:D:) {
        $!elems && nqp::elems($!elems)
          ?? nqp::create(MixHash).SET-SELF(
               Rakudo::QuantHash.BAGGY-CLONE($!elems))
          !! nqp::create(MixHash)
    }

    multi method Setty(Bag:U:) { Set      }
    multi method Setty(Bag:D:) { self.Set }
    multi method Baggy(Bag:U:) { Bag      }
    multi method Baggy(Bag:D:) { self     }
    multi method Mixy (Bag:U:) { Mix      }
    multi method Mixy (Bag:D:) { self.Mix }

#--- illegal methods
    proto method classify-list(|) {
        X::Immutable.new(:method<classify-list>, :typename(self.^name)).throw;
    }
    proto method categorize-list(|) {
        X::Immutable.new(:method<categorize-list>, :typename(self.^name)).throw;
    }
}

# vim: expandtab shiftwidth=4
