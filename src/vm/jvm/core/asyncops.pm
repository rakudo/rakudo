# Waits for a promise to be kept or a channel to be able to receive a value
# and, once it can, unwraps or returns the result. This should be made more
# efficient by using continuations to suspend any task running in the thread
# pool that blocks; for now, this cheat gets the basic idea in place.

proto sub await(|) { * }
multi sub await(Promise $p) {
    $p.result
}
multi sub await(*@awaitables) {
    @awaitables.eager.map(&await)
}
multi sub await(Channel $c) {
    $c.receive
}

my constant $WINNER_KIND_DONE = 0;
my constant $WINNER_KIND_MORE = 1;

sub WINNER(@winner_args, *@pieces, :$wild_done, :$wild_more, :$wait, :$wait_time is copy) {
    my num $start_time = nqp::time_n();
    my Int $num_pieces = +@pieces div 3;
    my $timeout_promise;
    sub invoke_right(&block, $key, $value?) {
        my @names = map *.name, &block.signature.params;
        return do if @names eqv ['$k', '$v'] || @names eqv ['$v', '$k'] {
            &block(:k($key), :v($value));
        } elsif @names eqv ['$_'] || (+@names == 1 && &block.signature.params[0].positional)  {
            &block($value);
        } elsif @names eqv ['$k'] {
            &block(:k($key));
        } elsif @names eqv ['$v'] {
            &block(:v($value));
        } elsif +@names == 0 {
            return &block();
        } else {
            die "couldn't figure out how to invoke {&block.signature().perl}";
        }
    }
    # if we don't have a last block, we need to retry until we
    # have a winner.
    my $action;
    loop {
        my @promises_only;
        my Bool $has_channels = False;
        if $num_pieces > 0 {
            for (^$num_pieces).pick(*) -> $index {
                my ($kind, $arg, &block) = @pieces[$index * 3, $index * 3 + 1, $index * 3 + 2];
                if $kind == $WINNER_KIND_DONE {
                    if $arg ~~ Promise {
                        if $arg {
                            $action = { invoke_right(&block, $arg, $arg.result) };
                            last;
                        }
                        @promises_only.push: $arg;
                    } elsif $arg ~~ Channel {
                        if $arg.closed {
                            $action = { invoke_right(&block, $arg); }
                            last;
                        }
                        $has_channels = True;
                    } else {
                        die "Got a {$arg.WHAT.perl}, but expected a Channel or Promise.";
                    }
                } elsif $kind == $WINNER_KIND_MORE {
                    if $arg ~~ Channel {
                        if (my $val := $arg.poll) !~~ Nil {
                            $action = { invoke_right(&block, $arg, $val); }
                            last;
                        }
                        $has_channels = True;
                    } elsif $arg ~~ Promise {
                        die "cannot use 'more' on a Promise.";
                    } else {
                        die "Got a {$arg.WHAT.perl}, but expected a Channel or Promise.";
                    }
                }
            }
            last if $action;
            if $wait {
                $wait_time -= (nqp::time_n() - $start_time);
                if $wait_time <= 0 || $timeout_promise {
                    $action = $wait;
                    last;
                } elsif !$timeout_promise {
                    $timeout_promise = Promise.in($wait_time);
                    @promises_only.push: $timeout_promise;
                    $num_pieces++;
                    @pieces.push: 0;
                    @pieces.push: $timeout_promise;
                    @pieces.push: $wait;
                }
            }
        } else {
            for @winner_args.pick(*) {
                when Channel {
                    if (my $val := $_.poll()) !~~ Nil {
                        $action = { invoke_right($wild_more, $_, $val) };
                        last;
                    } elsif $_.closed {
                        $action = { $wild_done(:k($_)); }
                        last;
                    }
                    $has_channels = True;
                }
                when Promise {
                    if $_ eqv $timeout_promise && $_ {
                        $action = $wait;
                        last;
                    } elsif $_ {
                        $action = { invoke_right($wild_done, $_, $_.result); }
                        last;
                    }
                    @promises_only.push: $_;
                }
                default {
                    die "Got a {$_.WHAT.perl}, but expected a Channel or Promise.";
                }
            }
            last if $action;
            # when we hit this, none of the promises or channels
            # have given us a result. if we have a wait closure,
            # we immediately return, otherwise we block on any
            # of the promises of our args.
            if $wait {
                $wait_time -= (nqp::time_n() - $start_time);
                if $wait_time <= 0 || $timeout_promise {
                    return $wait();
                } elsif !$timeout_promise {
                    $timeout_promise = Promise.in($wait_time);
                    @promises_only.push: $timeout_promise;
                    $num_pieces++;
                    @winner_args.push: $timeout_promise;
                }
            }
            # if we only had promises, we can block on "anyof".
        }
        if $has_channels || (@promises_only == 0) {
            Thread.yield();
        } else {
            Promise.anyof(|@promises_only).result;
        }
    }
    $action()
}
