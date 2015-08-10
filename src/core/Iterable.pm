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

    method flat() {
        Seq.new(class :: does Iterator {
            has $!source;
            has Iterator $!nested-iter;

            method new(\source-iter) {
                my \iter = self.CREATE;
                nqp::bindattr(iter, self, '$!source', source-iter);
                iter
            }

            method pull-one() {
                my $result;
                loop {
                    if $!nested-iter {
                        $result := $!nested-iter.pull-one();
                        last unless $result =:= IterationEnd;
                        $!nested-iter := Iterator;
                    }
                    $result := $!source.pull-one();
                    last unless nqp::istype($result, Iterable) && !nqp::iscont($result);
                    $!nested-iter := $result.flat.iterator;
                }
                $result
            }

            # This is a prime candidate for implementing most of the other
            # methods, for speed reasons
        }.new(self.iterator))
    }

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

            method pull-one() {
                unless $!iterator.DEFINITE {
                    $!iterator := $!iterable.iterator;
                }
                $!iterator.pull-one
            }

            method push-exactly($target, int $n) {
                unless $!iterator.DEFINITE {
                    $!iterator := $!iterable.iterator;
                }
                $!iterator.push-exactly($target, $n);
            }

            method lazy() { True }
        }.new(self))
    }

    method eager() {
        Seq.new(class :: does Iterator {
            has $!iterator;
            has $!cache;

            method new(\iterator) {
                my \iter = self.CREATE;
                nqp::bindattr(iter, self, '$!iterator', iterator);
                iter
            }

            method pull-one() {
                self!fill-cache() unless $!cache.DEFINITE;
                nqp::elems($!cache)
                    ?? nqp::shift($!cache)
                    !! IterationEnd
            }

            method push-exactly($target, int $n) {
                self!fill-cache() unless $!cache.DEFINITE;
                my $cache := $!cache;
                my int $todo = $n < nqp::elems($cache)
                    ?? $n
                    !! nqp::elems($cache);
                my int $i = 0;
                while $i < $n {
                    $target.push(nqp::shift($cache));
                    $i = $i + 1
                }
                nqp::elems($cache)
                    ?? $todo
                    !! IterationEnd
            }

            method push-at-least($target, int $n) {
                self!all($target)
            }

            method push-until-lazy($target) {
                self!all($target)
            }

            method push-all($target) {
                self!all($target)
            }

            method !fill-cache() {
                $!cache := IterationBuffer.CREATE;
                $!iterator.push-all($!cache);
            }

            method !all($target) {
                # Normally if we end up here, we didn't pull things into the
                # cache. But if we did, delegate to the default version of
                # push-all from the role to make sure we eat out the cache,
                # not from the now-depleted iterator.
                $!cache.DEFINITE
                    ?? self.Iterator::push-all($target)
                    !! $!iterator.push-all($target)
            }
        }.new(self.iterator))
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
