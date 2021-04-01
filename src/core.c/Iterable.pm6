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

    method hyper(
      Int(Cool) :$batch = 64,
      Int(Cool) :$degree = max(nqp::cpucores() - 1,1)
    ) {
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

    method race(
      Int(Cool) :$batch = 64,
      Int(Cool) :$degree = max(nqp::cpucores() - 1,1)
    ) {
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

    method !MIXIFY(\type) {
        (my \iterator := self.flat.iterator).is-lazy
          ?? type.fail-iterator-cannot-be-lazy('coerce')
          !! nqp::elems(my \elems := Rakudo::QuantHash.ADD-PAIRS-TO-MIX(
               nqp::create(Rakudo::Internals::IterationSet),iterator,Mu
             ))
            ?? nqp::create(type).SET-SELF(elems)
            !! nqp::eqaddr(type,Mix)
              ?? mix()
              !! nqp::create(type)
    }
    multi method Mix(Iterable:D:)     { self!MIXIFY(Mix)     }
    multi method MixHash(Iterable:D:) { self!MIXIFY(MixHash) }

    method !BAGGIFY(\type) {
        (my \iterator := self.flat.iterator).is-lazy
          ?? type.fail-iterator-cannot-be-lazy('coerce')
          !! nqp::elems(my \elems := Rakudo::QuantHash.ADD-PAIRS-TO-BAG(
               nqp::create(Rakudo::Internals::IterationSet),iterator,Mu
             ))
            ?? nqp::create(type).SET-SELF(elems)
            !! nqp::eqaddr(type,Bag)
              ?? bag()
              !! nqp::create(type)
    }
    multi method Bag(Iterable:D:)     { self!BAGGIFY(Bag)     }
    multi method BagHash(Iterable:D:) { self!BAGGIFY(BagHash) }

    method !SETIFY(\type) {
        (my \iterator := self.flat.iterator).is-lazy
          ?? type.fail-iterator-cannot-be-lazy('coerce')
          !! nqp::elems(my $elems := Rakudo::QuantHash.ADD-PAIRS-TO-SET(
               nqp::create(Rakudo::Internals::IterationSet),iterator,Mu
             ))
            ?? nqp::create(type).SET-SELF($elems)
            !! nqp::eqaddr(type,Set)
              ?? set()
              !! nqp::create(type)
    }
    multi method Set(Iterable:D:)     { self!SETIFY(Set)     }
    multi method SetHash(Iterable:D:) { self!SETIFY(SetHash) }
}

multi sub infix:<eqv>(Iterable:D \a, Iterable:D \b) {
    nqp::hllbool(
      nqp::unless(
        nqp::eqaddr(nqp::decont(a),nqp::decont(b)),
        nqp::if(                                 # not same object
          nqp::eqaddr(a.WHAT,b.WHAT),
          nqp::if(                               # same type
            nqp::istype(a, Positional),
            nqp::if(                             # Positional
              a.is-lazy,
              nqp::if(                           # a lazy
                b.is-lazy,
                Any.throw-iterator-cannot-be-lazy('eqv','') # a && b lazy
              ),
              nqp::if(                           # a NOT lazy
                b.is-lazy,
                0,                               # b lazy
                nqp::if(                         # a && b NOT lazy
                  nqp::iseq_i((my int $elems = a.elems),b.elems),
                  nqp::stmts(                    # same # elems
                    (my int $i = -1),
                    nqp::while(
                      nqp::islt_i(($i = nqp::add_i($i,1)),$elems) # not exhausted
                        && a.AT-POS($i) eqv b.AT-POS($i),         # still same
                      nqp::null
                    ),
                    nqp::iseq_i($i,$elems)       # exhausted = success!
                  )
                )
              )
            ),
            nqp::if(                             # NOT Positional
              nqp::iseq_i(
                (my \ia := a.iterator).is-lazy,
                (my \ib := b.iterator).is-lazy
              ),
              nqp::if(
                ia.is-lazy,
                Any.throw-iterator-cannot-be-lazy('eqv',''),
                nqp::stmts(
                  nqp::until(
                    nqp::stmts(
                      (my \pa := ia.pull-one),
                      (my \pb := ib.pull-one),
                      nqp::eqaddr(pa,IterationEnd)
                        || nqp::eqaddr(pb,IterationEnd)
                        || nqp::not_i(pa eqv pb)
                    ),
                    nqp::null
                  ),
                  nqp::eqaddr(pa,pb)     # both IterationEnd = success!
                )
              )
            )
          )
        )
      )
    )
}

#?if jvm
nqp::p6setitertype(Iterable);
#?endif

# vim: expandtab shiftwidth=4
