my role Iterator    { ... }

package Rakudo::Internals {

    our role MapIterator does Iterator {
        has $!hash-storage;
        has $!hash-iter;

        method BUILD(\hash) {
            $!hash-storage := nqp::getattr(hash, Map, '$!storage');
            $!hash-storage := nqp::hash() unless $!hash-storage.DEFINITE;
            $!hash-iter    := nqp::iterator($!hash-storage);
            self
        }
        method new(\hash) { nqp::create(self).BUILD(hash) }
        method count-only() {
            $!hash-iter := Mu;
            nqp::p6box_i(nqp::elems($!hash-storage))
        }
    }
}

# vim: ft=perl6 expandtab sw=4
