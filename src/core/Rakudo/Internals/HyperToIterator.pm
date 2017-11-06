my class Backtrace { ... }
my role X::HyperRace::Died {
    has $.start-backtrace;
    multi method gist(::?CLASS:D:) {
        "A worker in a parallel iteration (hyper or race) initiated here:\n" ~
            ((try $!start-backtrace ~ "\n") // '<unknown location>') ~
            "Died at:\n" ~
            callsame().indent(4)
    }
}

my class Rakudo::Internals::HyperToIterator does Rakudo::Internals::HyperJoiner does Iterator {
    has Channel $.batches .= new;

    has int $!last-target = -1;
    has int $!next-to-send = 0;
    has @!held-back;
    method consume-batch(Rakudo::Internals::HyperWorkBatch $batch --> Nil) {
        if $batch.last {
            $!last-target = $batch.sequence-number;
        }
        self!handle-batch($batch);
        if $!last-target >= 0 && $!next-to-send > $!last-target {
            $!batches.close;
        }
    }
    method !handle-batch($batch) {
        my int $seq = $batch.sequence-number;
        if $seq == $!next-to-send {
            $!batches.send($batch);
            ++$!next-to-send;
            if @!held-back {
                @!held-back.=sort(*.sequence-number);
                while @!held-back && @!held-back[0].sequence-number == $!next-to-send {
                    $!batches.send(@!held-back.shift);
                    ++$!next-to-send;
                }
            }
        }
        else {
            @!held-back.push($batch);
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
                default {
                    unless nqp::istype($_, X::HyperRace::Died) {
                        ($_ but X::HyperRace::Died(Backtrace.new(5))).rethrow
                    }
                }
            }
        }
        nqp::shift(nqp::decont($!current-items))
    }
}

# vim: ft=perl6 expandtab sw=4
