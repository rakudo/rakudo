my class Semaphore is repr('Semaphore') {
    method new(int $permits) {
        return nqp::box_i($permits, Semaphore);
    }
    method acquire() {
        nqp::semacquire(self);
    }
    method try_acquire() returns Bool {
        nqp::semtryacquire(self) ?? True !! False;
    }
    method release() {
        nqp::semrelease(self);
    }
}

# vim: ft=perl6 expandtab sw=4
