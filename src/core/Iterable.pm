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

    method item(Iterable \item:) is raw {
        nqp::iscont(item)
            ?? item
            !! nqp::p6bindattrinvres(nqp::create(Scalar), Scalar, '$!value', item)
    }

    method flat(Iterable:D:) {
        Seq.new(class :: does Iterator {
            has Iterator $!source;
            has Iterator $!nested;

            method new(\source) {
                nqp::p6bindattrinvres(nqp::create(self),self,'$!source',source)
            }

            my constant NO_RESULT_YET = nqp::create(Mu);
            method pull-one() is raw {
                my $result := NO_RESULT_YET;
                my $got;
                nqp::while(
                  nqp::eqaddr($result, NO_RESULT_YET),
                  nqp::if(
                    $!nested,
                    nqp::if(
                      nqp::eqaddr(($got := $!nested.pull-one),IterationEnd),
                      ($!nested := Iterator),
                      ($result := $got)
                    ),
                    nqp::if(
                      nqp::istype(($got := $!source.pull-one),Iterable)
                        && nqp::not_i(nqp::iscont($got)),
                      ($!nested := $got.flat.iterator),
                      ($result := $got)
                    )
                  )
                );
                $result
            }
            method push-all($target --> IterationEnd) {
                my $got;
                my $nested;
                nqp::until(
                  nqp::eqaddr(($got := $!source.pull-one),IterationEnd),
                  nqp::if(  # doesn't sink
                    nqp::istype($got,Iterable) && nqp::not_i(nqp::iscont($got)),
                    nqp::stmts(
                      ($nested := $got.flat.iterator),
                      nqp::until(  # doesn't sink
                        nqp::eqaddr(($got := $nested.pull-one),IterationEnd),
                        $target.push($got)
                      )
                    ),
                    $target.push($got)
                  )
                )
            }
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
          ?? Failure.new(X::Invalid::Value.new(
               :$method,:name<batch>,:value($batch)))
          !! $degree <= 0
            ?? Failure.new(X::Invalid::Value.new(
                 :$method,:name<degree>,:value($degree)))
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
}

#?if jvm
nqp::p6setitertype(Iterable);
#?endif

# vim: ft=perl6 expandtab sw=4
