multi sub next(\x --> Nil) { THROW(nqp::const::CONTROL_NEXT, x) }
multi sub last(\x --> Nil) { THROW(nqp::const::CONTROL_LAST, x) }

# vim: expandtab shiftwidth=4
