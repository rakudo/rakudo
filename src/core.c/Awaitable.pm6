# An Awaitable is something we can use the `await` operator on. To support
# this, it requires a `get-await-handle` method be implemented, which returns
# an `Awaitable::AwaitHandle`.
my role Awaitable {
    method get-await-handle() { ... }
}

# An Awaitable::Handle implementation is an immutable object that conveys the
# status of the requested asynchronous result at the point we obtain the
# handle. If the `.already` property is `True`, then there is no need to block
# or suspend execution; the `.result` or `.cause` of failure can be used right
# away (depending on the value of `.success). Otherwise, the consumer of the
# handle should call the `subscribe-awaiter` method with its unblock/resume
# handler, and then proceed to block/suspend. In this case, the handler will
# be passed two arguments: a `Bool` success, and a result/cause (result if
# success is `True`, cause if it's `False`). The `Awaitable::Handle` will
# *not* have its success/result/cause updated; this would open the door to
# data races (including subtle ones related to read/write ordering), when
# the point of the fast-path is to test if we've got a result already with
# minimal overhead (and thus minimal concurrency control).
my role Awaitable::Handle {
    has Bool $.already;
    has Bool $.success;
    has Mu $.result;
    has Exception $.cause;

    method already-success(Mu \result) {
        nqp::create(self)!ALREADY_SUCCESS(result)
    }
    method !ALREADY_SUCCESS(Mu \result) {
        $!already := $!success := True;
        $!result := result;
        self
    }

    method already-failure(Mu \cause) {
        nqp::create(self)!ALREADY_FAILURE(cause)
    }
    method !ALREADY_FAILURE(Mu \cause) {
        $!already := True;
        $!success := False;
        $!cause := cause;
        self
    }

    method subscribe-awaiter(&subscriber) { ... }
}

# vim: expandtab shiftwidth=4
