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

sub WINNER(@winner_args, *@pieces, :$wild_done, :$wild_more, :$wait) {
    my Int $num_pieces = +@pieces div 3;
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
    loop {
        my @promises_only;
        my Bool $has_channels = False;
        if $num_pieces > 0 {
            for (^$num_pieces).pick(*) -> $index {
                my ($kind, $arg, &block) = @pieces[$index * 3, $index * 3 + 1, $index * 3 + 2];
                if $kind == $WINNER_KIND_DONE {
                    if $arg ~~ Promise {
                        if $arg {
                            return invoke_right(&block, $arg, $arg.result);
                        }
                        @promises_only.push: $arg;
                    } elsif $arg ~~ Channel {
                        if $arg.closed {
                            return invoke_right(&block, $arg);
                        }
                        $has_channels = True;
                    } else {
                        die "Got a {$arg.WHAT.perl}, but expected a Channel or Promise.";
                    }
                } elsif $kind == $WINNER_KIND_MORE {
                    if $arg ~~ Channel {
                        if (my $val := $arg.poll) !~~ Nil {
                            return invoke_right(&block, $arg, $val);
                        }
                        $has_channels = True;
                    } elsif $arg ~~ Promise {
                        die "cannot use 'more' on a Promise.";
                    } else {
                        die "Got a {$arg.WHAT.perl}, but expected a Channel or Promise.";
                    }
                }
            }
            if $wait {
                return $wait();
            }
        } else {
            for @winner_args.pick(*) {
                when Channel {
                    if (my $val := $_.poll()) !~~ Nil {
                        return invoke_right($wild_more, $_, $val);
                    } elsif $_.closed.has_value {
                        return $wild_done(:k($_));
                    }
                    $has_channels = True;
                }
                when Promise {
                    if $_ {
                        return invoke_right($wild_done, $_, $_.result);
                    }
                    @promises_only.push: $_;
                }
                default {
                    die "Got a {$_.WHAT.perl}, but expected a Channel or Promise.";
                }
            }
            # when we hit this, none of the promises or channels
            # have given us a result. if we have a wait closure,
            # we immediately return, otherwise we block on any
            # of the promises of our args.
            if $wait {
                return $wait();
            }
            # if we only had promises, we can block on "anyof".
        }
        if $has_channels {
            Thread.yield();
        } else {
            Promise.anyof(@promises_only).result;
        }
    }
}
