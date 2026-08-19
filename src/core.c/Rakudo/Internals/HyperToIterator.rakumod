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

my class Rakudo::Internals::HyperToIterator
  does Rakudo::Internals::HyperJoiner
  does Iterator
  is implementation-detail
{
    has int $!seen-last;
    has int $!offset;
    has int $!stop-cutoff;
    has $!batches;
    has $!waiting;
    has $!current-items;

    my constant EMPTY_BUFFER = nqp::create(IterationBuffer);
    submethod TWEAK() {
        $!batches := Channel.new;
        $!waiting := nqp::list;
        $!current-items := EMPTY_BUFFER;
        $!stop-cutoff = -1;
    }

    method consume-batch(Rakudo::Internals::HyperWorkBatch $batch --> Nil) {
        my int $sequence = $batch.sequence-number;

        if $batch.stopped                      # loop control ended iteration
            && ($!stop-cutoff < 0 || $sequence < $!stop-cutoff) {
            $!stop-cutoff = $sequence;
            my int $keep = $!stop-cutoff + 1 - $!offset;
            nqp::setelems($!waiting,$keep)     # discard queued later batches
              if $keep >= 0 && nqp::isgt_i(nqp::elems($!waiting),$keep);
        }

        if $!stop-cutoff >= 0 && $sequence > $!stop-cutoff {
            self.batch-used();                 # dropped, but keep the
            return;                            # backpressure flowing
        }

        nqp::bindpos(                          # store the batch at its place
          $!waiting,
          nqp::sub_i($sequence,$!offset),
          $batch
        );

        nqp::until(                            # feed valid batches in order
          nqp::isnull(nqp::atpos($!waiting,0)),
          nqp::stmts(
            $!batches.send(nqp::shift($!waiting)),
            ++$!offset
          )
        );

        $!seen-last = 1                        # set flag we've seen last one
          if $batch.last
          || ($!stop-cutoff >= 0 && $!offset > $!stop-cutoff);
        $!batches.close                        # close channel if we're done
          if $!seen-last && nqp::not_i(nqp::elems($!waiting));
    }

    method consume-error(Exception $e --> Nil) {
        $!batches.fail($e);
    }

    method pull-one() is raw {
        until nqp::elems($!current-items) {      # handles empty batches
            CATCH {
                when X::Channel::ReceiveOnClosed {
                    return IterationEnd;
                }
                ($_ but X::HyperRace::Died(Backtrace.new(5))).rethrow
                  unless nqp::istype($_, X::HyperRace::Died);
            }
            $!current-items := $!batches.receive.items;
            self.batch-used();
        }
        nqp::shift($!current-items)
    }

    method skip-at-least(int $skipping) {
        my int $toskip = $skipping;
        while $toskip {
            CATCH {
                when X::Channel::ReceiveOnClosed {
                    return 0;
                }
                ($_ but X::HyperRace::Died(Backtrace.new(5))).rethrow
                  unless nqp::istype($_, X::HyperRace::Died);
            }
            if nqp::isge_i(nqp::elems($!current-items),$toskip) {
                nqp::splice($!current-items,EMPTY_BUFFER,0,$toskip);
                return 1;
            }
            $toskip = nqp::sub_i($toskip,nqp::elems($!current-items));
            $!current-items := $!batches.receive.items;
            self.batch-used();
        }
        0
    }

    method push-all(\target) {
        loop {
            CATCH {
                when X::Channel::ReceiveOnClosed {
                    return IterationEnd;
                }
                ($_ but X::HyperRace::Died(Backtrace.new(5))).rethrow
                  unless nqp::istype($_, X::HyperRace::Died);
            }
            target.append($!current-items);
            $!current-items := $!batches.receive.items;
            self.batch-used();
        }
    }
}

# vim: expandtab shiftwidth=4
