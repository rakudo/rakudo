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
    has $!closed;

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

    method send(Channel:D: \item) {
        X::Channel::SendOnClosed.new(channel => self).throw if $!closed;
        nqp::push($!queue, nqp::decont(item));
        $!async-notify.emit(True);
        Nil
    }

    method !receive(Channel:D: $fail-on-close) {
        my \msg := nqp::shift($!queue);
        if nqp::istype(msg, CHANNEL_CLOSE) {
            nqp::push($!queue, msg);  # make sure other readers see it
            $!closed_promise_vow.keep(Nil);
            X::Channel::ReceiveOnClosed.new(channel => self).throw
              if $fail-on-close;
            Nil
        }
        elsif nqp::istype(msg, CHANNEL_FAIL) {
            nqp::push($!queue, msg);  # make sure other readers see it
            $!closed_promise_vow.break(msg.error);
            die msg.error;
        }
        else {
            msg
        }
    }

    method receive(Channel:D:)              { self!receive(1) }
    method receive-nil-on-close(Channel:D:) { self!receive(0) }

    method poll(Channel:D:) {
        my \msg := nqp::queuepoll($!queue);
        if nqp::isnull(msg) {
            Nil
        } else {
            if nqp::istype(msg, CHANNEL_CLOSE) {
                $!closed_promise_vow.keep(Nil);
                Nil
            }
            elsif nqp::istype(msg, CHANNEL_FAIL) {
                $!closed_promise_vow.break(msg.error);
                Nil
            }
            else {
                msg
            }
        }
    }

    method !peek(Channel:D:) {
        my \msg := nqp::atpos($!queue, 0);
        if nqp::isnull(msg) {
            Nil
        } else {
            if nqp::istype(msg, CHANNEL_CLOSE) {
                $!closed_promise_vow.keep(Nil);
                Nil
            }
            elsif nqp::istype(msg, CHANNEL_FAIL) {
                $!closed_promise_vow.break(msg.error);
                Nil
            }
            else {
                msg
            }
        }
    }

    method Supply(Channel:D:) {
        supply {
            # Tap the async notification for new values supply.
            whenever $!async-notify.unsanitized-supply.schedule-on($*SCHEDULER) {
                my Mu \got = self.poll;
                if nqp::eqaddr(got, Nil) {
                    if $!closed_promise {
                        $!closed_promise.status == Kept
                            ?? done()
                            !! die $!closed_promise.cause
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
            # the supply. This lost event can then cause a deadlock.
            loop {
                my Mu \got = self.poll;
                last if nqp::eqaddr(got, Nil);
                emit got;
            }
            self!peek();
            if $!closed_promise {
                $!closed_promise.status == Kept
                    ?? done()
                    !! die $!closed_promise.cause
            }
        }
    }

    method iterator(Channel:D:) {
        class :: does Iterator {
            has $!channel;
            method !SET-SELF($!channel) { self }
            method new(\c) { nqp::create(self)!SET-SELF(c) }
            method pull-one() {
                my Mu \got = $!channel.receive-nil-on-close;
                nqp::eqaddr(got, Nil) ?? IterationEnd !! got
            }
        }.new(self)
    }

    method Seq(Channel:D:)  { Seq.new(self.iterator) }
    method list(Channel:D:) { self.Seq.list }

    my class ChannelAwaitableHandle does Awaitable::Handle {
        has $!channel;
        has $!closed_promise;
        has $!async-notify;

        method not-ready(Channel:D $channel, Promise:D $closed_promise, Supplier:D $async-notify) {
            self.CREATE!not-ready($channel, $closed_promise, $async-notify)
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
            # to take some care that we never call subscriber twice; a lock is
            # a tad heavy-weight for it, in the future we can just CAS an int.
            my $notified := False;
            my $l := Lock.new;
            my $t := $!async-notify.unsanitized-supply.tap: &poll-now;
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

    method get-await-handle(--> Awaitable::Handle) {
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

    method close() {
        $!closed = 1;
        nqp::push($!queue, CHANNEL_CLOSE);
        # if $!queue is otherwise empty, make sure that $!closed_promise
        # learns about the new value
        self!peek();
        $!async-notify.emit(True);
        Nil
    }

    method elems() {
        Failure.new("Cannot determine number of elements on a {self.^name}")
    }

    method fail($error is copy) {
        $!closed = 1;
        $error = X::AdHoc.new(payload => $error) unless nqp::istype($error, Exception);
        nqp::push($!queue, CHANNEL_FAIL.new(:$error));
        $!async-notify.emit(True);
        Nil
    }

    method closed() {
        self!peek();
        $!closed_promise
    }
}

# vim: ft=perl6 expandtab sw=4
