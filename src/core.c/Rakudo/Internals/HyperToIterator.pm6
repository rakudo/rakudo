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
    has $!current-items;

    my constant EMPTY_BUFFER = nqp::create(IterationBuffer);
    submethod TWEAK() {
        $!batches := Channel.new;
        $!waiting := nqp::list;
        $!current-items := EMPTY_BUFFER;
    }

    method consume-batch(Rakudo::Internals::HyperWorkBatch $batch --> Nil) {
        nqp::bindpos(                          # store the batch at its place
          $!waiting,
          nqp::sub_i($batch.sequence-number,$!offset),
          $batch
        );

        nqp::until(                            # feed valid batches in order
          nqp::isnull(nqp::atpos($!waiting,0)),
          nqp::stmts(
            $!batches.send(nqp::shift($!waiting)),
            ($!offset = nqp::add_i($!offset,1))
          )
        );

        $!seen-last = 1                        # set flag we've seen last one
          if $batch.last;
        $!batches.close                        # close channel if we're done
          if $!seen-last && nqp::not_i(nqp::elems($!waiting));
    }

    method consume-error(Exception $e --> Nil) {
        $!batches.fail($e);
    }

    method pull-one() is raw {
        until nqp::elems($!current-items) {      # handles empty batches
            $!current-items := $!batches.receive.items;
            self.batch-used();
            CATCH {
                when X::Channel::ReceiveOnClosed {
                    return IterationEnd;
                }
                ($_ but X::HyperRace::Died(Backtrace.new(5))).rethrow
                  unless nqp::istype($_, X::HyperRace::Died);
            }
        }
        nqp::shift($!current-items)
    }

    method skip-at-least(int $skipping) {
        my int $toskip = $skipping;
        while $toskip {
            if nqp::isge_i(nqp::elems($!current-items),$toskip) {
                nqp::splice($!current-items,EMPTY_BUFFER,0,$toskip);
                return 1;
            }
            $toskip = nqp::sub_i($toskip,nqp::elems($!current-items));
            $!current-items := $!batches.receive.items;
            self.batch-used();

            CATCH {
                when X::Channel::ReceiveOnClosed {
                    return 0;
                }
                ($_ but X::HyperRace::Died(Backtrace.new(5))).rethrow
                  unless nqp::istype($_, X::HyperRace::Died);
            }
        }
        0
    }

    method push-all(\target) {
        loop {
            target.append($!current-items);
            $!current-items := $!batches.receive.items;
            self.batch-used();

            CATCH {
                when X::Channel::ReceiveOnClosed {
                    return IterationEnd;
                }
                ($_ but X::HyperRace::Died(Backtrace.new(5))).rethrow
                  unless nqp::istype($_, X::HyperRace::Died);
            }
        }
    }
}

# vim: expandtab shiftwidth=4
