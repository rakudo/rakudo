my class Semaphore is repr('Semaphore') {
    method new(::?CLASS:_: uint $permits) {
        nqp::box_u($permits, Semaphore);
    }
    method acquire(::?CLASS:D: --> Nil) {
        nqp::semacquire(self);
    }
    method try_acquire(::?CLASS:D: --> Bool:D) {
        nqp::hllbool(nqp::semtryacquire(self))
    }
    method release(::?CLASS:D: --> Nil) {
        nqp::semrelease(self);
    }
    method Int(::?CLASS:D: --> Int:D) {
        nqp::unbox_u(self)
    }
    multi method Numeric(::?CLASS:D:) {
        self.Int
    }
}

# vim: expandtab shiftwidth=4
