my class Rakudo::Internals::RaceToIterator does Rakudo::Internals::HyperJoiner does Iterator {
    has Channel $.batches .= new;

    has int $!last-target = -1;
    has int $!batches-seen = 0;
    method consume-batch(Rakudo::Internals::HyperWorkBatch $batch --> Nil) {
        $!batches.send($batch);
        ++$!batches-seen;
        if $batch.last {
            $!last-target = $batch.sequence-number;
        }
        if $!last-target >= 0 && $!batches-seen == $!last-target + 1 {
            $!batches.close;
        }
    }

    method consume-error(Exception $e --> Nil) {
        $!batches.fail($e);
    }

    my constant EMPTY_BUFFER = IterationBuffer.CREATE;
    has IterationBuffer $!current-items = EMPTY_BUFFER;
    method pull-one() {
        until nqp::elems(nqp::decont($!current-items)) { # Handles empty batches
            my $batch = $!batches.receive;
            self.batch-used();
            $!current-items = $batch.items;
            CATCH {
                when X::Channel::ReceiveOnClosed {
                    return IterationEnd;
                }
                unless nqp::istype($_, X::HyperRace::Died) {
                    ($_ but X::HyperRace::Died(Backtrace.new(5))).rethrow
                }
            }
        }
        nqp::shift(nqp::decont($!current-items))
    }
}

# vim: expandtab shiftwidth=4
