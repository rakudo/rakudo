my class Semaphore is repr('Semaphore') {
    method new(int $permits) {
        nqp::box_i($permits, Semaphore);
    }
    method acquire() {
        nqp::semacquire(self);
    }
    method try_acquire(--> Bool:D) {
        nqp::hllbool(nqp::semtryacquire(self))
    }
    method release() {
        nqp::semrelease(self);
    }
}

# vim: expandtab shiftwidth=4
