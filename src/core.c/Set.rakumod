my class Set does Setty {
    has ValueObjAt $!WHICH;

    method ^parameterize(Mu \base, Mu \type) {
        my \what := base.^mixin(QuantHash::KeyOf[type]);
        what.^set_name(base.^name ~ '[' ~ type.^name ~ ']');
        what
    }

    multi method WHICH (Set:D: --> ValueObjAt:D) {
        nqp::isconcrete($!WHICH) ?? $!WHICH !! self!WHICH
    }

    method !WHICH() {
        $!WHICH := nqp::box_s(
          nqp::concat(
            nqp::if(
              nqp::eqaddr(self.WHAT,Set),
              'Set|',
              nqp::concat(nqp::unbox_s(self.^name), '|')
            ),
            nqp::sha1(
              nqp::join("\0",Rakudo::Sorting.MERGESORT-str(
                Rakudo::QuantHash.RAW-KEYS(self)
              ))
            )
          ),
          ValueObjAt
        )
    }

    my class Iterate does Rakudo::Iterator::Mappy {
        method pull-one() {
          $!iter
            ?? Pair.new(nqp::iterval(nqp::shift($!iter)),True)
            !! IterationEnd
        }
    }
    method iterator(Set:D:) { Iterate.new($!elems) }

    my class KV does Rakudo::QuantHash::Quanty-kv {
        method pull-one() is raw {
            nqp::if(
              $!on,
              nqp::stmts(
                ($!on = 0),
                True,
              ),
              nqp::if(
                $!iter,
                nqp::stmts(
                  ($!on = 1),
                  nqp::iterval(nqp::shift($!iter))
                ),
                IterationEnd
              )
            )
        }
        method push-all(\target --> IterationEnd) {
            nqp::while(
              $!iter,
              nqp::stmts(  # doesn't sink
                target.push(nqp::iterval(nqp::shift($!iter))),
                target.push(True)
              )
            )
        }
    }
    multi method kv(Set:D:) { Seq.new(KV.new(self)) }
    multi method values(Set:D:) { True xx self.total }

    multi method grab(Set:D: $count?) {
        X::Immutable.new( method => 'grab', typename => self.^name ).throw;
    }
    multi method grabpairs(Set:D: $count?) {
        X::Immutable.new( method => 'grabpairs', typename => self.^name ).throw;
    }

#--- stringification methods

    multi method gist(Set:D: --> Str:D) {
        nqp::concat(
          nqp::concat(
            nqp::concat(self.^name,'('),
            nqp::join(" ",
              Rakudo::Sorting.MERGESORT-str(
                Rakudo::QuantHash.RAW-VALUES-MAP(self, *.gist)
              )
            )
          ),
          ')'
        )
    }

    multi method raku(Set:D: --> Str:D) {
        nqp::if(
          nqp::eqaddr(self,set()),
          'set()',
          nqp::concat(
            nqp::concat(
              nqp::concat(self.^name,'.new('),
              nqp::join(",",Rakudo::QuantHash.RAW-VALUES-MAP(self, *.raku))
            ),
            ')'
          )
        )
    }

#--- coercion methods
    multi method Set(Set:D:) { self }
    multi method SetHash(Set:D:) {
        $!elems && nqp::elems($!elems)
          ?? nqp::p6bindattrinvres(
               nqp::create(SetHash),SetHash,'$!elems',nqp::clone($!elems)
             )
          !! nqp::create(SetHash)
    }

    multi method Setty(Set:U:) { Set      }
    multi method Setty(Set:D:) { self     }
    multi method Baggy(Set:U:) { Bag      }
    multi method Baggy(Set:D:) { self.Bag }
    multi method Mixy (Set:U:) { Mix      }
    multi method Mixy (Set:D:) { self.Mix }

#--- interface methods
    multi method STORE(Set:D: Any:D \keys, :INITIALIZE($)! --> Set:D) {
        (my \iterator := keys.iterator).is-lazy
          ?? self.fail-iterator-cannot-be-lazy('initialize')
          !! self.SET-SELF(Rakudo::QuantHash.ADD-PAIRS-TO-SET(
               nqp::create(Rakudo::Internals::IterationSet),
               iterator,
               self.keyof
             ))
    }
    multi method STORE(Set:D: \objects, \bools, :INITIALIZE($)! --> Set:D) {
        self.SET-SELF(
          Rakudo::QuantHash.ADD-OBJECTS-VALUES-TO-SET(
            nqp::create(Rakudo::Internals::IterationSet),
            objects.iterator,
            bools.iterator
          )
        )
    }

    multi method AT-KEY(Set:D: \k --> Bool:D) {
        nqp::hllbool($!elems ?? nqp::existskey($!elems,k.WHICH) !! 0)
    }
    multi method ASSIGN-KEY(Set:D: \k,\v) {
        X::Assignment::RO.new(value => self).throw;
    }
    multi method DELETE-KEY(Set:D: \k) {
        X::Immutable.new(method => 'DELETE-KEY', typename => self.^name).throw;
    }
}

# vim: expandtab shiftwidth=4
