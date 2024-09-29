#- Metamodel::Documenting ------------------------------------------------------
role Perl6::Metamodel::Documenting {
    has $!why;

    method WHY() { nqp::eqaddr($!why, NQPMu) ?? Nil !! $!why }

    method set_why($why) { $!why := $why }

    method is-implementation-detail($XXX?) { 0 }
}

# vim: expandtab sw=4
