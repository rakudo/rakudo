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
    has int $!seen-last;
    has int $!offset;
    has $!batches;
    has $!waiting;

    submethod TWEAK() {
        $!batches := Channel.new;
        $!waiting := nqp::list;
    }

    method consume-batch(Rakudo::Internals::HyperWorkBatch $batch --> Nil) {
        nqp::stmts(
          nqp::bindpos(                          # store the batch at its place
            $!waiting,
            nqp::sub_i($batch.sequence-number,$!offset),
            $batch
          ),
          nqp::until(                            # feed valid batches in order
            nqp::isnull(nqp::atpos($!waiting,0)),
            nqp::stmts(
              $!batches.send(nqp::shift($!waiting)),
              ($!offset = nqp::add_i($!offset,1))
            )
          ),
          nqp::if(                               # set flag we've seen last one
            $batch.last,
            ($!seen-last = 1)
          ),
          nqp::if(                               # close channel if we're done
            $!seen-last && nqp::not_i(nqp::elems($!waiting)),
            $!batches.close
          )
        )
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
