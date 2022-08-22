multi sub next(\x --> Nil) { THROW(nqp::const::CONTROL_NEXT, x) }
multi sub last(\x --> Nil) { THROW(nqp::const::CONTROL_LAST, x) }

proto sub prefix:<//>($) {*}
multi sub prefix:<//>(\a) { a.defined }

# vim: expandtab shiftwidth=4
