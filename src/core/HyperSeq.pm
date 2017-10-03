# A HyperSeq wraps up a HyperIterator. When asked for the hyper-iterator, it
# simply returns it, then complains if you ask a second time - much like Seq
# does for its iterator. If you ask for its iterator, then you are ending the
# declaration of a chain of parallelizable operations. That is, in fact, the
# thing that will actually kick off the parallel work.
my class HyperSeq does Iterable does HyperIterable does PositionalBindFailover {
    has HyperIterator $!hyper-iter;

    # The only valid way to create a HyperSeq directly is by giving it the
    # hyper-iterator it will expose and maybe memoize.
    method new(HyperIterator:D $hyper-iter) {
        nqp::p6bindattrinvres(
          nqp::create(self),HyperSeq,'$!hyper-iter',nqp::decont($hyper-iter)
        )
    }

    # Obtains the hyper-iterator (meaning we're being consumed as part of a
    # parallel processing pipeline).
    method hyper-iterator(HyperSeq:D:) {
        my \hyper-iter = $!hyper-iter;
        X::Seq::Consumed.new.throw unless hyper-iter.DEFINITE;
        $!hyper-iter := HyperIterator;
        hyper-iter
    }

    # Obtain the iterator, the consumption of which will kick off parallel
    # processing.
    method iterator(HyperSeq:D:) {
        class :: does Iterator {
            my constant NOT_STARTED = 0;
            my constant STARTED     = 1;
            my constant ALL_ADDED   = 2;

            # For concurrency control
            has $!lock;
            has $!cond-have-work;
            has $!cond-have-result;

            # State that must be protected by the above lock, used by all
            # threads involved.
            has $!work-available;
            has $!work-completed;
            has int $!in-progress;

            # State only touched by the thread controlling the iteration.
            has $!configuration;
            has $!hyper-iterator;
            has $!active-result-buffer;
            has $!status;
            has int $!sequence-number;

            has int $!next-result-sequence-number;

            method new(\hyper-iterator) {
                my \iter = nqp::create(self);
                my \lock = Lock.new;
                nqp::bindattr(iter, self, '$!hyper-iterator', hyper-iterator);
                nqp::bindattr(iter, self, '$!configuration', hyper-iterator.configuration);
                nqp::bindattr(iter, self, '$!work-available', nqp::create(IterationBuffer));
                nqp::bindattr(iter, self, '$!work-completed', nqp::create(IterationBuffer));
                nqp::bindattr(iter, self, '$!lock', lock);
                nqp::bindattr(iter, self, '$!cond-have-work', lock.condition);
                nqp::bindattr(iter, self, '$!cond-have-result', lock.condition);
                nqp::bindattr(iter, self, '$!status', NOT_STARTED);
                iter
            }

            method pull-one() {
                self!start() if $!status == NOT_STARTED;
                self!block-for-result() unless $!active-result-buffer.DEFINITE;
                if $!active-result-buffer.DEFINITE {
                    my \result = nqp::shift($!active-result-buffer);
                    $!active-result-buffer := Mu
                        unless nqp::elems($!active-result-buffer);
                    result
                }
                else {
                    IterationEnd
                }
            }

            method !start(--> Nil) {
                # Mark that we've started the work (done here because this
                # may get upgraded to ALL_ADDED if there's not much work).
                $!status := STARTED;

                # Add batches and start workers. Provided there is enough
                # work to do, this should feed them all nicely.
                for ^$!configuration.degree {
                    my \done = self!add-batch();
                    self!start-worker();
                    last if done =:= IterationEnd;
                }
            }

            method !add-batch() {
                my \work = HyperWorkBuffer.new;
                work.sequence-number = $!sequence-number++;
                # XXX error handling around below
                my \done = $!hyper-iterator.fill-buffer(work, $!configuration.batch);
                $!lock.protect({
                    nqp::push($!work-available, work);
                    if done =:= IterationEnd {
                        $!status := ALL_ADDED;
                        $!cond-have-work.signal_all();
                    } else {
                        $!cond-have-work.signal();
                    }
                });
                done
            }

            method !start-worker() {
                start {
                    loop {
                        # Acquire work.
                        my $my-work;
                        $!lock.protect({
                            until $my-work.DEFINITE {
                                if nqp::elems($!work-available) {
                                    $my-work := nqp::shift($!work-available);
                                    $!in-progress++;
                                }
                                elsif $!status == ALL_ADDED {
                                    last;
                                }
                                else {
                                    $!cond-have-work.wait();
                                }
                            }
                        });
                        unless $my-work.DEFINITE {
                            $!cond-have-result.signal();
                            last;
                        }

                        # Do work.
                        try {
                            $!hyper-iterator.process-buffer($my-work);
                            CATCH {
                                default {
                                    # GLR XXX error handling
                                    nqp::say(.gist);
                                }
                            }
                        }

                        # Place in results and signal anyone waiting for it.
                        $!lock.protect({
                            nqp::push($!work-completed, $my-work);
                            $!in-progress--;
                            $!cond-have-result.signal();
                        });
                    }
                }
            }

            method !block-for-result(--> Nil) {
                my int $we-got-an-empty-buffer;
                my int $last-amount-of-completed = 0;
                repeat while $we-got-an-empty-buffer {
                    my int $work-deficit = 0;
                    $we-got-an-empty-buffer = 0;
                    $!lock.protect({
                        until nqp::elems($!work-completed) > $last-amount-of-completed || self!finished() {
                            $!cond-have-result.wait();
                        }
                        my Mu $backlog := Mu;
                        while nqp::elems($!work-completed) && !$we-got-an-empty-buffer {
                            my $first-result := nqp::shift($!work-completed);
                            if $!configuration.race || $first-result.sequence-number == $!next-result-sequence-number {
                                $!active-result-buffer := $first-result.output;
                                $!next-result-sequence-number++;
                            } else {
                                if $backlog =:= Mu {
                                    $backlog := nqp::list();
                                }
                                nqp::push($backlog, $first-result);
                            }
                            $work-deficit = $!configuration.degree - nqp::elems($!work-available);
                            if $!active-result-buffer =:= Mu || $!active-result-buffer.elems == 0 {
                                $!active-result-buffer := Mu;
                                $we-got-an-empty-buffer = 1;
                            } else {
                                last;
                            }
                        }
                        unless $backlog =:= Mu {
                            while nqp::elems($backlog) {
                                nqp::push($!work-completed, nqp::shift($backlog));
                            }
                        }
                        $last-amount-of-completed = nqp::elems($!work-completed);
                    });
                    while $!status != ALL_ADDED && $work-deficit > 0 {
                        last if self!add-batch() =:= IterationEnd;
                        $work-deficit--;
                    }
                }
            }

            method !finished() {
                $!status == ALL_ADDED &&
                    nqp::elems($!work-available) == 0 &&
                    $!in-progress == 0
            }
        }.new(self.hyper-iterator)
    }

    # Various operations use the sequential iterator since they wish to set
    # off the parallel processing and consume the results.
    method List(HyperSeq:D:) {
        List.from-iterator(self.iterator)
    }
    method Slip(HyperSeq:D:) {
        Slip.from-iterator(self.iterator)
    }
    method Array(HyperSeq:D:) {
        Array.from-iterator(self.iterator)
    }
    method sink(HyperSeq:D: --> Nil) {
        # Means we're doing parallel work for its side-effects. Doesn't need
        # any special handling, nor does it warrant a warning since this is
        # what 'hyper for @xs -> $x { }' will end up calling.
        self.iterator.sink-all;
    }

    # Not indexable.
    multi method AT-POS(HyperSeq:D: $) {
        X::Seq::NotIndexable.new.throw
    }
    multi method EXISTS-POS(HyperSeq:D: $) {
        X::Seq::NotIndexable.new.throw
    }
    multi method DELETE-POS(HyperSeq:D: $) {
        X::Seq::NotIndexable.new.throw
    }
}

# vim: ft=perl6 expandtab sw=4
