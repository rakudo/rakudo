augment class Int {
    proto method roll(|) {*}
    multi method roll() { nqp::rand_I(self,Int) }
    multi method roll($count) { (^self).roll($count) }

    proto method pick(|) {*}
    multi method pick() { nqp::rand_I(self,Int) }
    multi method pick($count) { (^self).pick($count) }
}

# vim: expandtab shiftwidth=4
