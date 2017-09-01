my class Semaphore is repr('Semaphore') {
    method new(int $permits) {
        nqp::box_i($permits, Semaphore);
    }
    method acquire() {
        nqp::semacquire(self);
    }
    method try_acquire(--> Bool:D) {
        nqp::p6bool(nqp::semtryacquire(self))
    }
    method release() {
        nqp::semrelease(self);
    }
}

# vim: ft=perl6 expandtab sw=4
