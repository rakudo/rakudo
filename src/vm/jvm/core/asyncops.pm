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
# then the default block is ran. If there is no default block, select() blocks
# until one channel or promise is ready.
# If more than one channel/promise is ready, select() picks one at random
proto sub select(|) { * }
multi sub select(*@selectors, :$default) {
    multi is-ready(Promise $p) {
        if $p.has_result {
            return (True, $p)
        }
        return (False, False)
    }

    multi is-ready(Channel $c) {
        my $selected is default(Nil) = $c.poll;
        unless $selected === Nil {
            return (True, $selected)
        }
        return (False, False)
    }
    multi is-ready(Any $c) {
        die "Cannot use select on a " ~ .^name;
    }

    my $choice;
    loop {
        my @ready;
        my @waiting;
        for @selectors -> $s {
            die "select expects to be passed a list of pairs" unless $s ~~ Pair;
            my $arg = is-ready($s.key);
            if $arg[0] {
                @ready.push: $s.value => $arg[1]
            } else {
                @waiting.push: $s
            }
        }
        if @ready {
            $choice = @ready.pick;
            last;
        } elsif $default {
            $choice = $default;
            last;
        }
        else {
            Thread.yield;
        }
    }
    nqp::istype($choice, Pair)
        ?? $choice.key.($choice.value)
        !! $choice.()
}
