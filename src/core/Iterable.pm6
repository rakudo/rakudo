# Iterable is done by anything that we should be able to get an iterator
# from. Things that are Iterable will flatten in flattening contexts, so a
# default implementation of .flat is provided by this role. As itemization is
# what defeats flattening, this role also provides a default .item method.
# Additionally, as .lazy and .eager are about iterator behavior, they are
# provided by this role. Overriding those is not likely to be needed, and
# discouraged to maintain predictable semantics. Finally, both .hyper() and
# .race() are methods to enter the hyper and race paradigm and implemented
# here, so they can use any Iterable as a source.
my class HyperSeq { ... }
my class RaceSeq { ... }
my class Rakudo::Internals::HyperIteratorBatcher { ... }
my role Iterable {
    method iterator() { ... }

    method item() {
        nqp::p6bindattrinvres(nqp::create(Scalar), Scalar, '$!value', self)
    }

    my class Flat does Iterator {
        has Iterator $!source;
        has Iterator $!nested;

        method new(\source) {
            nqp::p6bindattrinvres(nqp::create(self),self,'$!source',source)
        }

        method pull-one() is raw {
            nqp::if(
              $!nested,
              nqp::if(
                nqp::eqaddr((my \nested := $!nested.pull-one),IterationEnd),
                nqp::stmts(
                  ($!nested := Iterator),
                  self.pull-one
                ),
                nested
              ),
              nqp::if(
                nqp::iscont(my \got := $!source.pull-one),
                got,
                nqp::if(
                  nqp::istype(got,Iterable),
                  nqp::stmts(
                    ($!nested := got.flat.iterator),
                    self.pull-one
                  ),
                  got
                )
              )
            )
        }

        method push-all(\target --> IterationEnd) {
            nqp::stmts(
              nqp::if(
                $!nested,
                nqp::stmts(
                  $!nested.push-all(target),
                  ($!nested := Iterator)
                )
              ),
              nqp::until(
                nqp::eqaddr((my \got := $!source.pull-one), IterationEnd),
                nqp::if(
                  nqp::iscont(got),
                  target.push(got),
                  nqp::if(
                    nqp::istype(got,Iterable),
                    got.flat.iterator.push-all(target),
                    target.push(got)
                  )
                )
              )
            )
        }
        method is-lazy() { $!source.is-lazy }
    }
    method flat(Iterable:D:) { Seq.new(Flat.new(self.iterator)) }

    method lazy-if($flag) { $flag ?? self.lazy !! self }

    my class Lazy does Iterator {
        has $!iterable;
        has $!iterator;

        method new(\iterable) {
            nqp::p6bindattrinvres(
              nqp::create(self),self,'$!iterable',iterable
            )
        }

        method pull-one() is raw {
            $!iterator := $!iterable.iterator unless $!iterator.DEFINITE;
            $!iterator.pull-one
        }

        method push-exactly(\target, int $n) {
            $!iterator := $!iterable.iterator unless $!iterator.DEFINITE;
            $!iterator.push-exactly(target, $n);
        }

        method is-lazy(--> True) { }
    }
    method lazy() {
        # Return a Seq with an iterator wrapping this Iterable, claiming to
        # be lazy, and implicitly preventing working ahead (by hiding any
        # push-at-least-n of the source iterator).
        Seq.new(Lazy.new(self))
    }

    method hyper(Int(Cool) :$batch = 64, Int(Cool) :$degree = 4) {
#?if !js
        HyperSeq.new:
          configuration =>
            HyperConfiguration.new(:$degree, :$batch, :method<hyper>),
          work-stage-head =>
            Rakudo::Internals::HyperIteratorBatcher.new(:$.iterator)
#?endif
#?if js
        HyperSeq.new($.iterator)
#?endif
    }

    method race(Int(Cool) :$batch = 64, Int(Cool) :$degree = 4) {
#?if !js
        RaceSeq.new:
          configuration =>
            HyperConfiguration.new(:$degree, :$batch, :method<race>),
          work-stage-head =>
            Rakudo::Internals::HyperIteratorBatcher.new(:$.iterator)
#?endif
#?if js
        RaceSeq.new($.iterator)
#?endif
    }

    sub MIXIFY(\iterable, \type) {
        nqp::if(
          (my \iterator := iterable.flat.iterator).is-lazy,
          Failure.new(X::Cannot::Lazy.new(:action<coerce>,:what(type.^name))),
          nqp::if(
            nqp::elems(my \elems := Rakudo::QuantHash.ADD-PAIRS-TO-MIX(
              nqp::create(Rakudo::Internals::IterationSet),iterator,Mu
            )),
            nqp::create(type).SET-SELF(elems),
            nqp::if(
              nqp::eqaddr(type,Mix),
              mix(),
              nqp::create(type)
            )
          )
        )
    }
    multi method Mix(Iterable:D:)     { MIXIFY(self, Mix)     }
    multi method MixHash(Iterable:D:) { MIXIFY(self, MixHash) }

    sub BAGGIFY(\iterable, \type) {
        nqp::if(
          (my \iterator := iterable.flat.iterator).is-lazy,
          Failure.new(X::Cannot::Lazy.new(:action<coerce>,:what(type.^name))),
          nqp::if(
            nqp::elems(my \elems := Rakudo::QuantHash.ADD-PAIRS-TO-BAG(
              nqp::create(Rakudo::Internals::IterationSet),iterator,Mu
            )),
            nqp::create(type).SET-SELF(elems),
            nqp::if(
              nqp::eqaddr(type,Bag),
              bag(),
              nqp::create(type)
            )
          )
        )
    }
    multi method Bag(Iterable:D:)     { BAGGIFY(self, Bag)     }
    multi method BagHash(Iterable:D:) { BAGGIFY(self, BagHash) }

    sub SETIFY(\iterable, \type) {
        nqp::if(
          (my \iterator := iterable.flat.iterator).is-lazy,
          Failure.new(X::Cannot::Lazy.new(:action<coerce>,:what(type.^name))),
          nqp::if(
            nqp::elems(my $elems := Rakudo::QuantHash.ADD-PAIRS-TO-SET(
              nqp::create(Rakudo::Internals::IterationSet),iterator,Mu
            )),
            nqp::create(type).SET-SELF($elems),
            nqp::if(
              nqp::eqaddr(type,Set),
              set(),
              nqp::create(type)
            )
          )
        )
    }
    multi method Set(Iterable:D:)     { SETIFY(self,Set)     }
    multi method SetHash(Iterable:D:) { SETIFY(self,SetHash) }
}

#?if jvm
nqp::p6setitertype(Iterable);
#?endif

# vim: ft=perl6 expandtab sw=4
