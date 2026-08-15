my class Rakudo::Internals::RaceToIterator does Rakudo::Internals::HyperJoiner does Iterator {
    has Channel $.batches .= new;

    has int $!target = -1;
    has int $!next-unseen = 0;
    has $!seen;

    submethod TWEAK() {
        $!seen := nqp::list_i;
    }

    # Closes the batches channel once every batch up to the target sequence
    # number has been consumed. The target is the batch the batcher marked
    # as last, or the earliest batch a loop control stopped the iteration
    # in; batches beyond a stop are dropped.
    method consume-batch(Rakudo::Internals::HyperWorkBatch $batch --> Nil) {
        my int $sequence = $batch.sequence-number;
        if ($batch.last || $batch.stopped)
            && ($!target < 0 || $sequence < $!target) {
            $!target = $sequence;
        }
        if $!target >= 0 && $sequence > $!target {
            self.batch-used();                 # dropped, but keep the
            return;                            # backpressure flowing
        }
        $!batches.send($batch);
        nqp::bindpos_i($!seen,$sequence,1);
        nqp::while(
          nqp::atpos_i($!seen,$!next-unseen),
          ++$!next-unseen
        );
        if $!target >= 0 && $!next-unseen > $!target {
            $!batches.close;
        }
    }

    method consume-error(Exception $e --> Nil) {
        $!batches.fail($e);
    }

    my constant EMPTY_BUFFER = nqp::create(IterationBuffer);
    has IterationBuffer $!current-items = EMPTY_BUFFER;
    method pull-one() {
        until nqp::elems(nqp::decont($!current-items)) { # Handles empty batches
            CATCH {
                when X::Channel::ReceiveOnClosed {
                    return IterationEnd;
                }
                unless nqp::istype($_, X::HyperRace::Died) {
                    ($_ but X::HyperRace::Died(Backtrace.new(5))).rethrow
                }
            }
            my $batch = $!batches.receive;
            self.batch-used();
            $!current-items = $batch.items;
        }
        nqp::shift(nqp::decont($!current-items))
    }
}

# vim: expandtab shiftwidth=4
