# Takes a linked list of pipeline stages and assembles them into a pipeline.
# Given a pipeline must end with a HyperJoiner, it expects to be passed
# something of this type.
my class Rakudo::Internals::HyperPipeline {
    # Records the earliest batch that a loop control stopped the iteration
    # in. Shared by the workers of one pipeline as a signal to cut the work
    # short: once set, the batcher stops producing and batches past the
    # cutoff are no longer processed. The joiners do not depend on it; they
    # terminate on the stop markers the batches themselves carry.
    my class StopStatus {
#?if moar
        has atomicint $!cutoff = -1;
#?endif
#?if !moar
        has int $!cutoff = -1;
#?endif

        method cutoff() { $!cutoff }

        method stop(int $sequence --> Nil) {
#?if moar
            loop {
                my int $current = $!cutoff;
                last if $current >= 0 && $current <= $sequence;
                last if nqp::cas_i($!cutoff, $current, $sequence) == $current;
            }
#?endif
#?if !moar
            $!cutoff = $sequence if $!cutoff < 0 || $sequence < $!cutoff;
#?endif
        }
    }

    method start(Rakudo::Internals::HyperJoiner $stage, HyperConfiguration $config) {
        # Create channel that the last non-join operation in the pipeline will
        # put its results into, and start a worker to handle the channel.
        my $cur-dest-channel = Channel.new;
        self!join-worker($stage, $cur-dest-channel);

        # Create a channel that will signal we're ready for more batches,
        # and set join stage to send on it when batch-used is called.
        my $ready-channel = Channel.new;
        $stage.SET-BATCH-USED-CHANNEL($ready-channel);

        # Shared by the stages of this pipeline that react to a loop
        # control stopping the iteration early.
        my $stop = StopStatus.new;

        # Go through the rest of the stages.
        my $cur-stage = $stage.source;
        my @processors;
        while $cur-stage {
            my $next-stage = $cur-stage.source;
            given $cur-stage {
                when Rakudo::Internals::HyperProcessor {
                    # Unshift them so a sequence will be in application order.
                    unshift @processors, $_;
                }
                when Rakudo::Internals::HyperBatcher {
                    if $next-stage {
                        die "A HyperBatcher may only be at the pipeline start";
                    }
                    $cur-dest-channel = self!maybe-processor-workers:
                        [@processors], $cur-dest-channel, $config.degree, $stop;
                    @processors = ();
                    self!batch-worker($cur-stage, $cur-dest-channel, $ready-channel,
                        $config.batch, $stop);
                }
                default {
                    die "Unrecognized hyper pipeline stage " ~ .^name();
                }
            }
            $cur-stage = $next-stage;
        }

        # Set off $degree batches.
        $ready-channel.send(True) for ^$config.degree;
    }

    method !batch-worker(Rakudo::Internals::HyperBatcher $stage, Channel $dest-channel,
                         Channel $ready-channel, int $size, StopStatus $stop) {
        start {
            my $AWAITER := $*AWAITER;
            loop {
                CATCH {
                    default {
                        $dest-channel.fail($_);
                    }
                }
                $AWAITER.await($ready-channel);
                last if $stop.cutoff >= 0;
                my $batch := $stage.produce-batch($size);
                $dest-channel.send($batch);
                last if $batch.last;
            }
        }
    }

    method !maybe-processor-workers(@processors, Channel $dest-channel, Int:D $degree,
                                    StopStatus $stop) {
        return $dest-channel unless @processors;
        my $source-channel := Channel.new;
        for ^$degree {
            start {
                CATCH {
                    when X::Channel::ReceiveOnClosed {
                        $dest-channel.close;
                    }
                    default {
                        $dest-channel.fail($_);
                    }
                }
                my $AWAITER := $*AWAITER;
                loop {
                    my $batch := $AWAITER.await($source-channel);
                    my int $cutoff = $stop.cutoff;
                    if $cutoff >= 0 && $batch.sequence-number > $cutoff {
                        # The iteration already stopped in an earlier batch;
                        # this one can contribute nothing.
                        $batch.replace-with(nqp::create(IterationBuffer));
                    }
                    else {
                        for @processors {
                            .process-batch($batch);
                        }
                        $stop.stop($batch.sequence-number) if $batch.stopped;
                    }
                    $dest-channel.send($batch);
                }
            }
        }
        return $source-channel;
    }

    method !join-worker(Rakudo::Internals::HyperJoiner $stage, Channel $source) {
        start {
            CATCH {
                when X::Channel::ReceiveOnClosed {
                    # We got everything; quietly exit the start block.
                }
                default {
                    $stage.consume-error($_);
                    CATCH {
                        default {
                            # Error handling code blew up; let the scheduler's
                            # error handler do it, which will typically bring
                            # the program down. Should never get here unless
                            # we've some bug in a joiner implementation.
                            $*SCHEDULER.handle_uncaught($_);
                        }
                    }
                }
            }
            my $AWAITER := $*AWAITER;
            loop {
                $stage.consume-batch($AWAITER.await($source));
            }
        }
    }
}

# vim: expandtab shiftwidth=4
