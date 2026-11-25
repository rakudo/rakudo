#- Metamodel::BUILDALL ---------------------------------------------------------
# Provides the .new method for a class, based on BUILDALL logic

role Perl6::Metamodel::BUILDALL {

    my &BUILDALL := nqp::findmethod(NQPMu, 'BUILDALL');

    method new(*%named) {
        BUILDALL(nqp::create(self), %named)
    }
}

# vim: expandtab sw=4
