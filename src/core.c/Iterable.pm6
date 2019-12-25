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

    method flat(Iterable:D:) { Seq.new(Rakudo::Iterator.Flat(self.iterator)) }

    method lazy-if($flag) { $flag ?? self.lazy !! self }

    method lazy() {
        # Return a Seq with an iterator wrapping this Iterable, claiming to
        # be lazy, and implicitly preventing working ahead (by hiding any
        # push-at-least-n of the source iterator).
        Seq.new(Rakudo::Iterator.Lazy(self))
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
