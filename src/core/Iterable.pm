# Iterable is done by anything that we should be able to get an iterator
# from. Things that are Iterable will flatten in flattening contexts, so a
# default implementation of .flat is provided by this role. As itemization is
# what defeats flattening, this role also provides a default .item method.
# Additionally, as .lazy and .eager are about iterator behavior, they are
# provided by this role. Overriding those is not likely to be needed, and
# discouraged to maintain predictable semantics. Finally, both .hyper() and
# .race() are implemented here, and return a HyperSeq wrapping the iterator.
my class Seq { ... }
my class HyperSeq { ... }
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
                my \iter = nqp::create(self);
                nqp::bindattr(iter, self, '$!source', source);
                iter
            }

            my constant NO_RESULT_YET = Mu.CREATE;
            method pull-one() is raw {
                my $result := NO_RESULT_YET;
                my $got;
                while nqp::eqaddr($result, NO_RESULT_YET) {
                    if $!nested {
                        $got := $!nested.pull-one;
                        nqp::eqaddr($got, IterationEnd)
                          ?? ($!nested := Iterator)
                          !! ($result := $got);
                    }
                    else {
                        $got := $!source.pull-one();
                        nqp::istype($got, Iterable) && !nqp::iscont($got)
                          ?? ($!nested := $got.flat.iterator)
                          !! ($result := $got);
                    }
                }
                $result
            }
            method push-all($target) {
                my $got;
                until ($got := $!source.pull-one) =:= IterationEnd {
                    if nqp::istype($got, Iterable) && !nqp::iscont($got) {
                        my $nested := $got.flat.iterator;
                        $target.push($got)
                          until ($got := $nested.pull-one) =:= IterationEnd;
                    }
                    else {
                        $target.push($got);
                    }
                }
                IterationEnd
            }
            method count-only() {
                my int $found;
                my $got;
                until ($got := $!source.pull-one) =:= IterationEnd {
                    if nqp::istype($got, Iterable) && !nqp::iscont($got) {
                        my $nested := $got.flat.iterator;
                        $found = $found + 1
                          until ($got := $nested.pull-one) =:= IterationEnd;
                    }
                    else {
                        $found = $found + 1;
                    }
                }
                nqp::p6box_i($found)
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
                my \iter = self.CREATE;
                nqp::bindattr(iter, self, '$!iterable', iterable);
                iter
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

    method hyper(Int(Cool) :$batch = 64, Int(Cool) :$degree = 4) {
        self!go-hyper(HyperConfiguration.new(:!race, :$batch, :$degree))
    }

    method race(Int(Cool) :$batch = 64, Int(Cool) :$degree = 4) {
        self!go-hyper(HyperConfiguration.new(:race, :$batch, :$degree))
    }

    method !go-hyper($configuration) {
        HyperSeq.new(class :: does HyperIterator {
            has $!source;
            has $!configuration;

            method new(\iter, $configuration) {
                my \hyper-iter = self.CREATE;
                nqp::bindattr(hyper-iter, self, '$!source', iter);
                nqp::bindattr(hyper-iter, self, '$!configuration', $configuration);
                hyper-iter
            }

            method fill-buffer(HyperWorkBuffer:D $work, int $items) {
                $!source.push-exactly($work.input, $items)
            }

            method process-buffer(HyperWorkBuffer:D $work) {
                Mu
            }

            method configuration() { $!configuration }
        }.new(self.iterator, $configuration));
    }
}

# vim: ft=perl6 expandtab sw=4
