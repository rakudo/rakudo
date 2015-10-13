# Waits for a promise to be kept or a channel to be able to receive a value
# and, once it can, unwraps or returns the result. This should be made more
# efficient by using continuations to suspend any task running in the thread
# pool that blocks; for now, this cheat gets the basic idea in place.

proto sub await(|) { * }
multi sub await() {
    die "Must specify a Promise or Channel to await on (got an empty list)";
}
multi sub await(Any $x) {
    die "Must specify a Promise or Channel to await on (got a $x.^name())";
}
multi sub await(Iterable:D $i) { $i.eager.map({ await $_ }) }
multi sub await(Promise:D $p)  { $p.result }
multi sub await(Channel:D $c)  { $c.receive }
multi sub await(*@awaitables)  { @awaitables.eager.map({await $_}) }

sub cas (\val,&code) { val = code(val) } # naive implementation of cas

sub INVOKE_KV(&block, $key, $value?) {

    my @names = map *.name, &block.signature.params;

    if @names eqv ['$k', '$v'] || @names eqv ['$v', '$k'] {
        &block(:k($key), :v($value));
    }
    elsif @names eqv ['$_'] || (+@names == 1 && &block.signature.params[0].positional)  {
        &block($value);
    }
    elsif @names eqv ['$k'] {
        &block(:k($key));
    }
    elsif @names eqv ['$v'] {
        &block(:v($value));
    }
    elsif +@names == 0 {
        &block();
    }
    else {
        die "Couldn't figure out how to invoke {&block.signature().perl}";
    }
}

sub EARLIEST(@earliest,*@other,:$wild_done,:$wild_more,:$wait,:$wait_time) {
    my Num $until = $wait ?? nqp::time_n() + $wait_time !! Nil;

    my constant $EARLIEST_KIND_DONE = 0;
    my constant $EARLIEST_KIND_MORE = 1;

    my @todo;
#       |-- [ ordinal, kind, contestant, block, alternate_block? ]
    my %distinct-channels;
    my %channels-by-kind;

    # sanity check and transmogrify possibly multiple channels into things to do
    while +@other {
        my $kind = @other.shift;
        if $kind != $EARLIEST_KIND_DONE && $kind != $EARLIEST_KIND_MORE {
            die "Got a {$kind.WHAT.perl}, but expected $EARLIEST_KIND_DONE or $EARLIEST_KIND_MORE";
        }

        my @contestant;
        while !nqp::istype(@other[0],Block) {
            my $next := @other.shift;
            if !nqp::istype($next,Channel) {
                die "Got a {$next.WHAT.perl}, but expected a Channel";
            }
            @contestant.push: $next;
        }
        my &block = @other.shift;

        for @contestant {
            %channels-by-kind{$kind}{$_.WHICH} = $_;
            @todo.push: [ +@todo, $kind, $_, &block ];
        }
    }

    %distinct-channels = %channels-by-kind.values>>.pairs.flat;

    # for the channels specified directly to the earliest then
    # if they have not already got a done or more then add one

    for @earliest {
        when Channel {
            my $n = $_.WHICH;

            if $wild_more.defined {
                if not %channels-by-kind{$EARLIEST_KIND_MORE}{$n}:exists {
                    @todo.push: [ +@todo, $EARLIEST_KIND_MORE, $_, $wild_more ];
                    %distinct-channels{$n} = $_;
                }
            }
            if $wild_done.defined {
                if not %channels-by-kind{$EARLIEST_KIND_DONE}{$n}:exists {
                    @todo.push: [ +@todo, $EARLIEST_KIND_DONE, $_, $wild_done ];
                    %distinct-channels{$n} = $_;
                }
            }
        }
        default {
            die "Got a {$_.WHAT.perl}, but expected a Channel";
        }
    }

    if !@todo {
        die "Nothing todo for earliest";
    }

    my $action;

    CHECK:
    loop {  # until something to return
        for @todo.pick(+@todo) -> $todo {
            my $kind       := $todo[1];
            my $contestant := $todo[2];

            if $kind == $EARLIEST_KIND_DONE {
                if $contestant.closed {
                    $action = { INVOKE_KV($todo[3], $todo[0]) };
                    last CHECK;
                }
            }

            else { # $kind == $EARLIEST_KIND_MORE
                if +%distinct-channels == 1 && !$wait {
                    try {
                        my $value := $contestant.receive;
                        $action = { INVOKE_KV($todo[3], $todo[0], $value) };
                        last CHECK;

                        CATCH {
                            when X::Channel::ReceiveOnClosed {
                                if $todo[4] {
                                    $action = { INVOKE_KV($todo[4], $todo[0]) };
                                    last CHECK;
                                }
                            }
                        }
                    }
                }
                else {
                    if !nqp::istype((my $value := $contestant.poll),Nil) {
                        $action = { INVOKE_KV($todo[3], $todo[0], $value) };
                        last CHECK;
                    }

                    elsif $contestant.closed && $todo[4] {
                        $action = { INVOKE_KV($todo[4], $todo[0]) };
                        last CHECK;
                    }
                }
            }
        }

        # we have to wait
        if $until && nqp::time_n() >= $until {  # we're done waiting
            $action = $wait;
            last CHECK;
        }

        # wait a bit
        Thread.yield()
    }

    # must do action outside above loop to make any "last" in block find the right loop
    $action();
}

# vim: ft=perl6 expandtab sw=4
