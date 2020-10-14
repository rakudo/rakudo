# IterationBuffer is used when the list/iteration implementation needs a
# lightweight way to store/transmit values. Replaces the use of nqp::list in
# the list guts, which is an impediment to introspectability and also to
# allowing the implementation of custom iterators (though in reality most
# folks won't implement Iterator directly, but instead use gather/take or lazy
# loops). It doesn't make Scalar containers, and only supports mutation
# through implementing push and BIND-POS, and access by implementing AT-POS.
# Hot-paths are free to use the nqp:: op set directly on this, and do things
# outside the scope of the method API it exposes. This type is engineered for
# performance over friendliness, and should not be encountered in normal use
# of Raku. Do NOT add any checks and validation to methods in here. They
# need to remain trivially inlineable for performance reasons.
my class IterationBuffer {
    method clear(IterationBuffer:D: --> Nil) {
        nqp::setelems(self, 0)
    }

    method elems() { nqp::elems(self) }

    method push(Mu \value) { nqp::push(self, value) }

    method append(IterationBuffer:D \buffer) {
        nqp::splice(self,buffer,nqp::elems(self),0)
    }

    proto method AT-POS(|) {*}
    multi method AT-POS(IterationBuffer:D: int $pos) is raw {
        nqp::atpos(self, $pos)
    }
    multi method AT-POS(IterationBuffer:D: Int:D $pos) is raw {
        nqp::atpos(self, $pos)
    }

    proto method BIND-POS(|) {*}
    multi method BIND-POS(IterationBuffer:D: int $pos, Mu \value) {
        nqp::bindpos(self, $pos, value)
    }
    multi method BIND-POS(IterationBuffer:D: Int:D $pos, Mu \value) {
        nqp::bindpos(self, $pos, value)
    }

    # For maintainability mainly, and possibly for creating smaller, more
    # inlineable candidates
    method Slip(IterationBuffer:D:) {
        nqp::p6bindattrinvres(nqp::create(Slip),List,'$!reified',self)
    }

    # For maintainability mainly, and possibly for creating smaller, more
    # inlineable candidates
    method List(IterationBuffer:D:) {
        nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',self)
    }

    # For maintainability mainly, and possibly for creating smaller, more
    # inlineable candidates
    method Seq(IterationBuffer:D:) {
        Seq.new(Rakudo::Iterator.ReifiedList(self))
    }

    # For maintainability mainly, and possibly for creating smaller, more
    # inlineable candidates
    method iterator(IterationBuffer:D:) {
        Rakudo::Iterator.ReifiedList(self)
    }

    # For core debugging purposes only: basically warp the IterationBuffer
    # into a full-fledged List and .raku that.  We don't care that it will
    # not round-trip.
    multi method raku(IterationBuffer:D:) {
        self.List.raku ~ '.IterationBuffer'
    }
}

#?if jvm
nqp::p6setiterbuftype(IterationBuffer);
#?endif

# vim: expandtab shiftwidth=4
