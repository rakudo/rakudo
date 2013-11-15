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

# Takes a list of pairs, mapping a Channel or Promise to code. Invokes the
# code block of whichever Channel receives first whichever Promise is kept
# or broken first. Evaluates to the result of that code block.
# If none of the channels have a value or none of the promises have a result,
# then the default block is ran. If there is no default block, winner() blocks
# until one channel or promise is ready.
# If more than one channel/promise is ready, winner() picks one at random

proto sub winner(|) { * }
multi sub winner(*@contestants, :$default) {
    multi is-ready(Promise $contestant) {
        if $contestant.has_result {
            return (True, $contestant)
        }
        return (False, False)
    }

    multi is-ready(Channel $c) {
        my $contestant is default(Nil) = $c.poll;
        unless $contestant === Nil {
            return (True, $contestant)
        }
        return (False, False)
    }
    multi is-ready(Any $c) {
        die "Cannot use winner on a " ~ .^name;
    }

    if @contestants.grep: { $_ !~~ Pair } {
       die "winner() expects to be passed a list of pairs";
    }

    my $winner;
    loop {
        for @contestants.pick(+@contestants) -> $contestant {
            next unless (my $arg = is-ready($contestant.key))[0];

            $winner = $contestant.value => $arg[1];
            last;
        }
        last if $winner //= $default;

        Thread.yield;
    }
    nqp::istype($winner, Pair)
        ?? $winner.key.($winner.value)
        !! $winner.()
}
