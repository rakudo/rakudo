# Takes a linked list of pipeline stages and assembles them into a pipeline.
# Given a pipeline must end with a HyperJoiner, it expects to be passed
# something of this type.
my class Rakudo::Internals::HyperPipeline {
    method start(Rakudo::Internals::HyperJoiner $stage, HyperConfiguration $config) {
        # Create channel that the last non-join operation in the pipeline will
        # put its results into, and start a worker to handle the channel.
        my $cur-dest-channel = Channel.new;
        self!join-worker($stage, $cur-dest-channel);

        # Create a channel that will signal we're ready for more batches,
        # and set join stage to send on it when batch-used is called.
        my $ready-channel = Channel.new;
        $stage.SET-BATCH-USED-CHANNEL($ready-channel);

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
                        [@processors], $cur-dest-channel, $config.degree;
                    @processors = ();
                    self!batch-worker($cur-stage, $cur-dest-channel, $ready-channel,
                        $config.batch);
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
                         Channel $ready-channel, int $size) {
        start {
            my $AWAITER := $*AWAITER;
            loop {
                $AWAITER.await($ready-channel);
                my $batch := $stage.produce-batch($size);
                $dest-channel.send($batch);
                last if $batch.last;
                CATCH {
                    default {
                        $dest-channel.fail($_);
                    }
                }
            }
        }
    }

    method !maybe-processor-workers(@processors, Channel $dest-channel, Int:D $degree) {
        return $dest-channel unless @processors;
        my $source-channel := Channel.new;
        for ^$degree {
            start {
                my $AWAITER := $*AWAITER;
                loop {
                    my $batch := $AWAITER.await($source-channel);
                    for @processors {
                        .process-batch($batch);
                    }
                    $dest-channel.send($batch);
                }
                CATCH {
                    when X::Channel::ReceiveOnClosed {
                        $dest-channel.close;
                    }
                    default {
                        $dest-channel.fail($_);
                    }
                }
            }
        }
        return $source-channel;
    }

    method !join-worker(Rakudo::Internals::HyperJoiner $stage, Channel $source) {
        start {
            my $AWAITER := $*AWAITER;
            loop {
                $stage.consume-batch($AWAITER.await($source));
            }
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
        }
    }
}

# vim: expandtab shiftwidth=4
