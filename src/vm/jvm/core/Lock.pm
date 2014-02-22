# A simple, reentrant lock mechanism.
my class Lock is repr('ReentrantMutex') {
    method lock() { nqp::lock(self) }

    method unlock() { nqp::unlock(self) }

    method protect(&code) {
        nqp::lock(self);
        my \res := code();
        nqp::unlock(self);
        CATCH { nqp::unlock(self); }
        res
    }
}
