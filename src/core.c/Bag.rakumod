my class Bag does Baggy {
    has ValueObjAt $!WHICH;
    has Int        $!total;

    method ^parameterize(Mu \base, Mu \type) {
        my \what := base.^mixin(QuantHash::KeyOf[type]);
        what.^set_name(
          nqp::concat(base.^name,'[') ~ nqp::concat(type.^name,']')
        );
        what
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
        $!total // ($!total := Rakudo::QuantHash.BAG-TOTAL($!elems))
    }


#--- interface methods
    multi method STORE(Bag:D: Any:D \keys, :INITIALIZE($)! --> Bag:D) {
        (my \iterator := keys.iterator).is-lazy
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

#--- stringification methods

    multi method gist(Bag:D: --> Str:D) {
        nqp::concat(
          nqp::concat(
            nqp::concat(self.^name,'('),
            nqp::join(' ',
              Rakudo::Sorting.MERGESORT-str(
                Rakudo::QuantHash.RAW-VALUES-MAP(self, {
                    (my \value := nqp::getattr($_,Pair,'$!value')) == 1
                      ?? nqp::getattr($_,Pair,'$!key').gist
                      !! "{nqp::getattr($_,Pair,'$!key').gist}({value})"
                })
              )
            )
          ),
          ')',
        )
    }

    multi method raku(Bag:D: --> Str:D) {
        nqp::if(
          $!elems && nqp::elems($!elems),
          nqp::stmts(
            (my \pairs := nqp::join(',',
              Rakudo::QuantHash.RAW-VALUES-MAP(self, {
                  nqp::concat(
                    nqp::concat(
                      nqp::getattr($_,Pair,'$!key').raku,
                      '=>'
                    ),
                    nqp::getattr($_,Pair,'$!value').raku
                  )
              })
            )),
            nqp::if(
              nqp::eqaddr(self.keyof,Mu),
              nqp::concat(
                nqp::concat('(',pairs),
                nqp::concat(').',self.^name)
              ),
              nqp::concat(
                nqp::concat(self.^name,'.new-from-pairs('),
                nqp::concat(pairs,')')
              )
            )
          ),
          nqp::if(
            nqp::eqaddr(self,bag()),
            'bag()',
            nqp::concat('().',self.^name)
          )
        )
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
