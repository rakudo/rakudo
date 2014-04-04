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

sub INVOKE_KV(&block, $key, $value?) {

    my @names = map *.name, &block.signature.params;

    if @names eqv ['$k', '$v'] || @names eqv ['$v', '$k'] {
        return &block(:k($key), :v($value));
    }
    elsif @names eqv ['$_'] || (+@names == 1 && &block.signature.params[0].positional)  {
        return &block($value);
    }
    elsif @names eqv ['$k'] {
        return &block(:k($key));
    }
    elsif @names eqv ['$v'] {
        return &block(:v($value));
    }
    elsif +@names == 0 {
        return &block();
    }

    die "Couldn't figure out how to invoke {&block.signature().perl}";
}

sub WINNER(@winner, *@other, :$wild_done, :$wild_more, :$wait, :$wait_time is copy) {
    my Num $until = $wait ?? nqp::time_n() + $wait !! Nil;

    my constant $WINNER_KIND_DONE = 0;
    my constant $WINNER_KIND_MORE = 1;

    my @todo;
#       |-- [ ordinal, kind, contestant, block, alternate_block? ]

    # sanity check and transmogrify possibly multiple promises into things to do
    while +@other {
        my $kind = @other.shift;
        if $kind != $WINNER_KIND_DONE && $kind != $WINNER_KIND_MORE {
            die "Got a {$kind.WHAT.perl}, but expected $WINNER_KIND_DONE or $WINNER_KIND_MORE";
        }

        my @contestant;
        while @other[0] !~~ Block {
            my $next := @other.shift;
            if $next !~~ Promise && $next !~~ Channel {
                die "Got a {$next.WHAT.perl}, but expected a Promise or Channel";
            }
            elsif $kind == $WINNER_KIND_MORE && $next ~~ Promise {
                die "Cannot use 'more' on a Promise";
            }
            push @contestant, $next;
        }
        my &block = @other.shift;

        @todo.push: [ +@todo, $kind, $_, &block ] for @contestant;
    }

    # transmogrify any winner spec if nothing to do so far
    if !@todo {
        for @winner {
            when Promise {
                @todo.push: [ +@todo, $WINNER_KIND_DONE, $_, $wild_done ];
            }
            when Channel {
                @todo.push: [ +@todo, $WINNER_KIND_MORE, $_, $wild_more, $wild_done ];
            }
            default {
                die "Got a {$_.WHAT.perl}, but expected a Promise or Channel";
            }
        }
    }

    if !@todo {
        die "Nothing todo for winner";
    }

    my $action;
    my $timeout_promise;

    CHECK:
    loop {  # until something to return
        my @promises;
        my Bool $must_yield;

        for @todo.pick(*) -> $todo {
            my $kind       := $todo[1];
            my $contestant := $todo[2];

            if $kind == $WINNER_KIND_DONE {

                if $contestant ~~ Promise {
                    if $contestant {   # kept/broken
                        $action = {
                            INVOKE_KV(
                              $todo[3],
                              $todo[0],
                              $contestant.status == Kept
                                ?? $contestant.result
                                !! fail $contestant.excuse,  # Broken
                            );
                        };
                        last; # CHECK;
                    }
                    @promises.push: $contestant;
                }

                else {   # Channel
                    if $contestant.closed {
                        $action = { INVOKE_KV($todo[3], $todo[0]) };
                        last; # CHECK;
                    }
                }
            }

            else { # $kind == $WINNER_KIND_MORE && $contestant ~~ Channel

                if (my $value := $contestant.poll) !~~ Nil {
                    $action = { INVOKE_KV($todo[3], $todo[0], $value) };
                    last; # CHECK;
                }

                elsif $contestant.closed && $todo[4] {
                    $action = { INVOKE_KV($todo[4], $todo[0]) };
                    last; # CHECK;
                }
                $must_yield = True;
            }
        }

        last if $action; # remove if we can last to CHECK:

        # we have to wait
        if $until {
            if $nqp::time_n() >= $until {  # we're done waiting
                $action = $wait;
                last; # CHECK;
            }
            
            # make sure we wait next time
            @promises.push: $timeout_promise //= Promise.at($until);
        }

        # yield the thread only if we must
        $must_yield
          ?? Thread.yield()
          !! Promise.anyof(|@promises).result;
    }

    # must do action outside above loop to make any "last" in block find the right loop
    $action();
}
