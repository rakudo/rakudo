# A simple, reentrant lock mechanism.
my class Lock {
    has $!lock;

    submethod BUILD() {
        my \ReentrantLock := nqp::jvmbootinterop().typeForName('java.util.concurrent.locks.ReentrantLock');
        $!lock            := ReentrantLock.'constructor/new/()V'();
    }

    method lock() { $!lock.lock() }

    method unlock() { $!lock.unlock() }

    method protect(&code) {
        $!lock.lock();
        my \res := code();
        $!lock.unlock();
        CATCH { $!lock.unlock(); }
        res
    }
}
