# Iterable is done by anything that we should be able to get an iterator
# from. Things that are Iterable will flatten in flattening contexts, so a
# default implementation of .flat is provided by this role. As itemization is
# what defeats flattening, this role also provides a default .item method.
# Additionally, as .lazy and .eager are about iterator behavior, they are
# provided by this role. Overriding those is not likely to be needed, and
# discouraged to maintain predictable semantics. Finally, both .hyper() and
# .race() are implemented here, and return a HyperSeq wrapping the iterator.
my class HyperSeq { ... }
my class X::Invalid::Value { ... }
my role Iterable {
    method iterator() { ... }

    method item() {
        nqp::p6bindattrinvres(nqp::create(Scalar), Scalar, '$!value', self)
    }

    method flat(Iterable:D:) {
        Seq.new(class :: does Iterator {
            has Iterator $!source;
            has Iterator $!nested;

            method new(\source) {
                nqp::p6bindattrinvres(nqp::create(self),self,'$!source',source)
            }

            method pull-one() is raw {
                nqp::if(
                  $!nested,
                  nqp::if(
                    nqp::eqaddr((my $got := $!nested.pull-one),IterationEnd),
                    nqp::stmts(
                      ($!nested := Iterator),
                      self.pull-one
                    ),
                    $got
                  ),
                  nqp::if(
                    nqp::iscont($got := $!source.pull-one),
                    $got,
                    nqp::if(
                      nqp::istype($got,Iterable),
                      nqp::stmts(
                        ($!nested := $got.flat.iterator),
                        self.pull-one
                      ),
                      $got
                    )
                  )
                )
            }

            method push-all($target --> IterationEnd) {
                nqp::stmts(
                  nqp::if(
                    $!nested,
                    nqp::stmts(
                      $!nested.push-all($target),
                      ($!nested := Iterator)
                    )
                  ),
                  nqp::until(
                    nqp::eqaddr((my $got := $!source.pull-one), IterationEnd),
                    nqp::if(
                      nqp::iscont($got),
                      $target.push($got),
                      nqp::if(
                        nqp::istype($got,Iterable),
                        $got.flat.iterator.push-all($target),
                        $target.push($got)
                      )
                    )
                  )
                )
            }
            method is-lazy() { $!source.is-lazy }
        }.new(self.iterator))
    }

    method lazy-if($flag) { $flag ?? self.lazy !! self }

    method lazy() {
        # Return a Seq with an iterator wrapping this Iterable, claiming to
        # be lazy, and implicitly preventing working ahead (by hiding any
        # push-at-least-n of the source iterator).
        Seq.new(class :: does Iterator {
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

            method push-exactly($target, int $n) {
                $!iterator := $!iterable.iterator unless $!iterator.DEFINITE;
                $!iterator.push-exactly($target, $n);
            }

            method is-lazy() { True }
        }.new(self))
    }

    method !valid-hyper-race($method,$batch,$degree --> Nil) {
        $batch <= 0
          ?? X::Invalid::Value.new(
               :$method,:name<batch>,:value($batch)).throw
          !! $degree <= 0
            ?? X::Invalid::Value.new(
                 :$method,:name<degree>,:value($degree)).throw
            !! Nil
    }

    method hyper(Int(Cool) :$batch = 64, Int(Cool) :$degree = 4) {
        self!valid-hyper-race('hyper',$batch,$degree);
        self!go-hyper(HyperConfiguration.new(:!race, :$batch, :$degree))
    }

    method race(Int(Cool) :$batch = 64, Int(Cool) :$degree = 4) {
        self!valid-hyper-race('race',$batch,$degree);
        self!go-hyper(HyperConfiguration.new(:race, :$batch, :$degree))
    }

    method !go-hyper($configuration) {
        HyperSeq.new(class :: does HyperIterator {
            has $!source;
            has $!configuration;

            method new(\iter, $configuration) {
                my \hyper-iter = nqp::create(self);
                nqp::bindattr(hyper-iter, self, '$!source', iter);
                nqp::bindattr(hyper-iter, self, '$!configuration', $configuration);
                hyper-iter
            }

            method fill-buffer(HyperWorkBuffer:D $work, int $items) {
                $!source.push-exactly($work.input, $items)
            }

            method process-buffer(HyperWorkBuffer:D $work --> Nil) { }

            method configuration() { $!configuration }
        }.new(self.iterator, $configuration));
    }


    method !SETIFY(\type) {
        nqp::create(type).SET-SELF(
          type.fill_IterationSet(
            nqp::create(Rakudo::Internals::IterationSet),self.flat.iterator
          )
        )
    }
    multi method Set(Iterable:D:)     { self!SETIFY(Set)     }
    multi method SetHash(Iterable:D:) { self!SETIFY(SetHash) }
}

#?if !moar
nqp::p6setitertype(Iterable);
#?endif

# vim: ft=perl6 expandtab sw=4
