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
my class Channel {
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

    method receive(Channel:D:) {
        my \msg := nqp::shift($!queue);
        if nqp::istype(msg, CHANNEL_CLOSE) {
            nqp::push($!queue, msg);  # make sure other readers see it
            $!closed_promise_vow.keep(Nil);
            X::Channel::ReceiveOnClosed.new(channel => self).throw
        }
        elsif nqp::istype(msg, CHANNEL_FAIL) {
            nqp::push($!queue, msg);  # make sure other readers see it
            $!closed_promise_vow.break(msg.error);
            die msg.error;
        }
        msg
    }

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
            loop {
                my Mu \got = self.poll;
                last if nqp::eqaddr(got, Nil);
                emit got;
            }
            self!peek();
            done() if $!closed_promise;
            whenever $!async-notify.unsanitized-supply.schedule-on($*SCHEDULER) {
                my Mu \got = self.poll;
                if nqp::eqaddr(got, Nil) {
                    done() if $!closed_promise;
                }
                else {
                    emit got;
                }
            }
        }
    }

    multi method list(Channel:D:) {
        self.Supply.list
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
