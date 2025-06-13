my class Mix does Mixy {
    has ValueObjAt $!WHICH;
    has Real       $!total;
    has Real       $!total-positive;

    method ^parameterize(Mu \base, Mu \type) {
        my \what := base.^mixin(QuantHash::KeyOf[type]);
        what.^set_name(
          nqp::concat(base.^name,'[') ~ nqp::concat(type.^name,']')
        );
        what
    }

#--- interface methods
    multi method STORE(Mix:D: Any:D \keys, :INITIALIZE($)! --> Mix:D) {
        (my \iterator := keys.iterator).is-lazy
          ?? self.fail-iterator-cannot-be-lazy('.initialize')
          !! self.SET-SELF(Rakudo::QuantHash.ADD-PAIRS-TO-MIX(
               nqp::create(Rakudo::Internals::IterationSet),iterator,self.keyof
             ))
    }
    multi method STORE(Mix:D: \objects, \values, :INITIALIZE($)! --> Mix:D) {
        self.SET-SELF(
          Rakudo::QuantHash.ADD-OBJECTS-VALUES-TO-MIX(
            nqp::create(Rakudo::Internals::IterationSet),
            objects.iterator,
            values.iterator,
            self.keyof
          )
        )
    }

    multi method DELETE-KEY(Mix:D: $) {
        X::Immutable.new(method => 'DELETE-KEY', typename => self.^name).throw;
    }

#--- introspection methods
    multi method WHICH(Mix:D: --> ValueObjAt:D) {
        nqp::isconcrete($!WHICH) ?? $!WHICH !! self!WHICH
    }

    method !WHICH() {
        $!WHICH := nqp::box_s(
          nqp::concat(
            nqp::if(
              nqp::eqaddr(self.WHAT,Mix),
              'Mix|',
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
    method total(Mix:D: --> Real:D) {
        $!total // ($!total := Rakudo::QuantHash.MIX-TOTAL($!elems))
    }
    method !total-positive(Mix:D: --> Real:D) {
        $!total-positive // ($!total-positive := Rakudo::QuantHash.MIX-TOTAL-POSITIVE($!elems))
    }

#--- selection methods
    multi method grab($count? --> Real:D) {
        X::Immutable.new( method => 'grab', typename => self.^name ).throw;
    }
    multi method grabpairs($count? --> Real:D) {
        X::Immutable.new( method => 'grabpairs', typename => self.^name ).throw;
    }

#--- stringification methods

    multi method gist(Mix:D: --> Str:D) {
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

    multi method raku(Mix:D: --> Str:D) {
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
            nqp::eqaddr(self,mix()),
            'mix()',
            nqp::concat('().',self.^name)
          )
        )
    }

#--- coercion methods
    multi method Mix(Mix:D:) { self }
    multi method MixHash(Mix:D:) {
        $!elems && nqp::elems($!elems)
          ?? nqp::create(MixHash).SET-SELF(
               Rakudo::QuantHash.BAGGY-CLONE($!elems))
          !! nqp::create(MixHash)
    }

    multi method Setty(Mix:U:) { Set }
    multi method Setty(Mix:D:) { self.Set }
    multi method Baggy(Mix:U:) { Bag }
    multi method Baggy(Mix:D:) { self.Bag }
    multi method Mixy (Mix:U:) { Mix }
    multi method Mixy (Mix:D:) { self }

#--- illegal methods
    proto method classify-list(|) {
        X::Immutable.new(:method<classify-list>, :typename(self.^name)).throw;
    }
    proto method categorize-list(|) {
        X::Immutable.new(:method<categorize-list>, :typename(self.^name)).throw;
    }
}

# vim: expandtab shiftwidth=4
