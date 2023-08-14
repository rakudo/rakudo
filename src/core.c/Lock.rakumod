# A reentrant lock mechanism with condition variable support.
my class X::Lock::ConditionVariable::New is Exception {
    method message() {
        "Cannot directly create a ConditionVariable; use the 'condition' method on a lock"
    }
}
my class Lock {
    class ConditionVariable is repr('ConditionVariable') {
        method new() {
            X::Lock::ConditionVariable::New.new.throw
        }
        proto method wait(|) {*}
        multi method wait(--> Nil) { nqp::condwait(self) }
        multi method wait(&predicate --> Nil) { nqp::condwait(self) until predicate; }
        method signal() { nqp::condsignalone(self) }
        method signal_all() { nqp::condsignalall(self) }
    }

    method new() { nqp::create(self) }

    method lock(Lock:D:) { nqp::lock(self) }

    method unlock(Lock:D:) { nqp::unlock(self) }

    # use a multi to ensure LEAVE isn't run when bad args are given
    proto method protect(|) {*}
    multi method protect(Lock:D: &code) is raw {
        nqp::lock(self);
        LEAVE nqp::unlock(self);
        code()
    }

    method condition(Lock:D:) {
        nqp::getlockcondvar(self, ConditionVariable)
    }
}

# vim: expandtab shiftwidth=4
