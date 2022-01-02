# A channel provides a thread-safe way to send a series of values from some
# producer(s) to some consumer(s).
my class X::Channel::SendOnClosed is Exception {
    has $.channel;
    method message() { "Cannot send a message on a closed channel" }
}
my class X::Channel::ReceiveOnClosed is Exception {
    has $.channel;
    method message() { "Cannot receive a message on a closed channel" }
}
my class Channel does Awaitable {
    # The queue of events moving through the channel.
    my class Queue is repr('ConcBlockingQueue') { }
    has $!queue;

    # Promise that is triggered when all values are received, or an error is
    # received and the channel is thus closed.
    has $!closed_promise;

    # Closed promise's vow.
    has $!closed_promise_vow;

    # Flag for if the channel is closed to senders.
    has int $!closed;

    # We use a Supplier to send async notifications that there may be a new
    # message to read from the channel (there may be many things competing
    # over them).
    has $!async-notify;

    # Magical objects for various ways a channel can end.
    my class CHANNEL_CLOSE { }
    my class CHANNEL_FAIL  { has $.error }

    submethod BUILD(--> Nil) {
        $!queue := nqp::create(Queue);
        $!closed_promise = Promise.new;
        $!closed_promise_vow = $!closed_promise.vow;
        $!async-notify = Supplier.new;
    }

    method send(Channel:D: \item --> Nil) {
        nqp::if(
          $!closed,
          X::Channel::SendOnClosed.new(channel => self).throw,
          nqp::stmts(
            nqp::push($!queue,nqp::decont(item)),
            $!async-notify.emit(True)
          )
        )
    }

    method receive(Channel:D:) {
        my \msg := nqp::shift($!queue);
        nqp::if(
          nqp::istype(msg,CHANNEL_CLOSE),
          nqp::stmts(
            nqp::push($!queue, msg),    # make sure other readers see it
            X::Channel::ReceiveOnClosed.new(channel => self).throw
          ),
          nqp::if(
            nqp::istype(msg,CHANNEL_FAIL),
            nqp::stmts(
              nqp::push($!queue,msg),   # make sure other readers see it
              msg.error.rethrow
            ),
            nqp::stmts(
              self!peek(),              # trigger promise if closed
              msg
            )
          )
        )
    }

    method poll(Channel:D:) {
        nqp::if(
          nqp::isnull(my \msg := nqp::queuepoll($!queue)),
          Nil,
          nqp::if(
            nqp::istype(msg, CHANNEL_CLOSE),
            nqp::stmts(
              nqp::push($!queue, msg),
              Nil
            ),
            nqp::if(
              nqp::istype(msg, CHANNEL_FAIL),
              nqp::stmts(
                nqp::push($!queue, msg),
                Nil
              ),
              nqp::stmts(
                self!peek(),              # trigger promise if closed
                msg
              )
            )
          )
        )
    }

    method !peek(Channel:D:) {
        my \msg := nqp::atpos($!queue, 0);
        if nqp::isnull(msg) {
            Nil
        } else {
            if nqp::istype(msg, CHANNEL_CLOSE) {
                try $!closed_promise_vow.keep(Nil);
                Nil
            }
            elsif nqp::istype(msg, CHANNEL_FAIL) {
                try $!closed_promise_vow.break(msg.error);
                Nil
            }
            else {
                msg
            }
        }
    }

    method Capture(Channel:D:) { self.List.Capture }
    multi method Supply(Channel:D:) {
        supply {
            my $closed = False;

            # Tap the async notification for new values supply.
            whenever $!async-notify.unsanitized-supply.schedule-on($*SCHEDULER) {
                done if $closed;
                my Mu \got = self.poll;
                if nqp::eqaddr(got, Nil) {
                    if $!closed_promise {
                        $!closed_promise.status == Kept
                            ?? done()
                            !! X::AdHoc.new( payload => $!closed_promise.cause ).throw
                    }
                }
                else {
                    emit got;
                }
            }

            # Grab anything that's in the channel and emit it. Note that
            # it's important to do this after tapping the supply, or a
            # value sent between us draining it and doing the tap would
            # not result in a notification, and so we'd not emit it on
            # the supply. This lost event can then cause a deadlock. We
            # also limit ourselves to fetching up to the number of items
            # currently in the channel before we started; any further
            # ones will result in an async notification. If we don't, and
            # the code we `emit` to itself synchronously adds things, then
            # we can end up with the async notifications piling up becuase
            # the `whenever` above never gets chance to run. Note that we
            # may be competing over the items currently in the queue, so the
            # `last if ...` check in this loop is still essential.
            my int $initial-items = nqp::elems($!queue);
            while $initial-items-- {
                done if $closed;
                my Mu \got = self.poll;
                last if nqp::eqaddr(got, Nil);
                emit got;
            }
            self!peek();
            if $!closed_promise {
                $!closed_promise.status == Kept
                    ?? done()
                    !! X::AdHoc.new( payload => $!closed_promise.cause ).throw
            }

            CLOSE {
                $closed = True;
            }
        }
    }

    my class Iterate { ... }
    trusts Iterate;
    my class Iterate does Iterator {
        has $!queue   is built(:bind);
        has $!channel is built(:bind);
        method pull-one() {
            my \msg := nqp::shift($!queue);
            nqp::if(
              nqp::istype(msg,CHANNEL_CLOSE),
              nqp::stmts(
                nqp::push($!queue,msg),     # make sure other readers see it
                IterationEnd
              ),
              nqp::if(
                nqp::istype(msg,CHANNEL_FAIL),
                nqp::stmts(
                  nqp::push($!queue,msg),   # make sure other readers see it
                  msg.error.rethrow
                ),
                nqp::stmts(
                  $!channel!Channel::peek(),    # trigger promise if closed
                  msg
                )
              )
            )
        }
    }
    method iterator(Channel:D:) { Iterate.new(:$!queue,:channel(self)) }

    method list(Channel:D:) { List.from-iterator: self.iterator }

    my class ChannelAwaitableHandle does Awaitable::Handle {
        has $!channel;
        has $!closed_promise;
        has $!async-notify;

        method not-ready(Channel:D $channel, Promise:D $closed_promise, Supplier:D $async-notify) {
            nqp::create(self)!not-ready($channel, $closed_promise, $async-notify)
        }
        method !not-ready($channel, $closed_promise, $async-notify) {
            $!already = False;
            $!channel := $channel;
            $!closed_promise := $closed_promise;
            $!async-notify := $async-notify;
            self
        }

        method subscribe-awaiter(&subscriber --> Nil) {
            # Need some care here to avoid a race. We must tap the notification
            # supply first, and then do an immediate poll after it, just to be
            # sure we won't miss notifications between the two. Also, we need
            # to take some care that we never call subscriber twice.
            my $notified := False;
            my $l := Lock.new;
            my $t;
            $l.protect: {
                # Lock ensures $t will be assigned before we run the logic
                # inside of poll-now, which relies on being able to do
                # $t.close.
                $t := $!async-notify.unsanitized-supply.tap: &poll-now;
            }
            poll-now();

            sub poll-now($discard?) {
                $l.protect: {
                    unless $notified {
                        my \maybe = $!channel.poll;
                        if maybe === Nil {
                            if $!closed_promise.status == Kept {
                                $notified := True;
                                subscriber(False, X::Channel::ReceiveOnClosed.new(:$!channel))
                            }
                            elsif $!closed_promise.status == Broken {
                                $notified := True;
                                subscriber(False, $!closed_promise.cause)
                            }
                        }
                        else {
                            $notified := True;
                            subscriber(True, maybe);
                        }
                        $t.close if $notified;
                    }
                }
            }
        }
    }

    method get-await-handle(--> Awaitable::Handle:D) {
        my \maybe = self.poll;
        if maybe === Nil {
            if $!closed_promise {
                ChannelAwaitableHandle.already-failure(
                    $!closed_promise.status == Kept
                        ?? X::Channel::ReceiveOnClosed.new(channel => self)
                        !! $!closed_promise.cause
                )
            }
            else {
                ChannelAwaitableHandle.not-ready(self, $!closed_promise, $!async-notify)
            }
        }
        else {
            ChannelAwaitableHandle.already-success(maybe)
        }
    }

    method close(--> Nil) {
        $!closed = 1;
        nqp::push($!queue, CHANNEL_CLOSE);
        # if $!queue is otherwise empty, make sure that $!closed_promise
        # learns about the new value
        self!peek();
        $!async-notify.emit(True);
    }

    method elems() {
        Failure.new("Cannot determine number of elements on a {self.^name}")
    }

    method fail($error is copy) {
        $!closed = 1;
        $error = X::AdHoc.new(payload => $error) unless nqp::istype($error, Exception);
        nqp::push($!queue, CHANNEL_FAIL.new(:$error));
        self!peek();
        $!async-notify.emit(True);
        Nil
    }

    method closed() {
        $!closed_promise
    }
}

# vim: expandtab shiftwidth=4
