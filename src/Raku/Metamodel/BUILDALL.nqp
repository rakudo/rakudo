#- Metamodel::BUILDALL ---------------------------------------------------------
# Provides the .new method for a class, based on BUILDALL logic

role Metamodel::BUILDALL {

    my &BUILDALL := nqp::findmethod(NQPMu, 'BUILDALL');

    method new(*%named) {
        BUILDALL(nqp::create(self), %named)
    }
}

# vim: expandtab sw=4
