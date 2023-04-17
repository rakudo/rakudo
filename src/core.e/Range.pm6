augment class Range {
    multi method Bool(Range:D: --> Bool:D) {
        nqp::hllbool($!is-int
          ?? ($!max - $!excludes-max - $!min - $!excludes-min) > -1
          !! nqp::not_i(nqp::eqaddr(self.iterator.pull-one,IterationEnd))
        )
    }
}

# vim: expandtab shiftwidth=4
