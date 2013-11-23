# A channel provides a thread-safe way to send a series of values from some
# producer(s) to some consumer(s).
my class X::Channel::SendOnClosed is Exception {
    method message() { "Cannot send a message on a closed channel" }
}
my class X::Channel::ReceiveOnClosed is Exception {
    method message() { "Cannot receive a message on a closed channel" }
}
my class Channel {
    # The queue of events moving through the channel.
    has Mu $!queue;
    
    # Promise that is triggered when all values are received, or an error is
    # received and the channel is thus closed.
    has $!closed_promise;
    
    # Closed promise's vow.
    has $!closed_promise_vow;
    
    # Flag for if the channel is closed to senders.
    has $!closed;
    
    # Magical objects for various ways a channel can end.
    my class CHANNEL_CLOSE { }
    my class CHANNEL_FAIL  { has $.error }
    
    my Mu $interop;
    submethod BUILD() {
        $interop := nqp::jvmbootinterop() unless nqp::isconcrete($interop);
        my \LinkedBlockingQueue := $interop.typeForName('java.util.concurrent.LinkedBlockingQueue');
        $!queue := LinkedBlockingQueue.'constructor/new/()V'();
        $!closed_promise = Promise.new;
        $!closed_promise_vow = $!closed_promise.vow;
    }
    
    method send(Channel:D: \item) {
        X::Channel::SendOnClosed.new.throw if $!closed;
        $!queue.add($interop.sixmodelToJavaObject(nqp::decont(item)))
    }
    
    method receive(Channel:D:) {
        my \msg := $interop.javaObjectToSixmodel($!queue.take());
        if nqp::istype(msg, CHANNEL_CLOSE) {
            $!closed_promise_vow.keep(Nil);
            X::Channel::ReceiveOnClosed.new.throw
        }
        elsif nqp::istype(msg, CHANNEL_FAIL) {
            $!closed_promise_vow.break(msg.error);
            die msg.error;
        }
        msg
    }
    
    method poll(Channel:D:) {
        my \fetched := $!queue.'method/poll/()Ljava/lang/Object;'();
        if nqp::jvmisnull(fetched) {
            Nil
        } else {
            my \msg := $interop.javaObjectToSixmodel(fetched);
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
        my \fetched := $!queue.'method/peek/()Ljava/lang/Object;'();
        if nqp::jvmisnull(fetched) {
            Nil
        } else {
            my \msg := $interop.javaObjectToSixmodel(fetched);
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

    method list($self:) {
        map {
            winner $self {
              more * { $_ }
              done * { last }
            }
        }, 0..*;  # until we have a listless map { }
    }

    method close() {
        $!closed = 1;
        $!queue.add($interop.sixmodelToJavaObject(CHANNEL_CLOSE))
    }
    
    method fail($error is copy) {
        $!closed = 1;
        $error = X::AdHoc.new(payload => $error) unless nqp::istype($error, Exception);
        $!queue.add($interop.sixmodelToJavaObject(CHANNEL_FAIL.new(:$error)))
    }
    
    method closed() {
        self!peek();
        $!closed_promise
    }
}
