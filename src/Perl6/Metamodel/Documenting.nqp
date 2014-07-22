role Perl6::Metamodel::Documenting {
    has $!why;

    method WHY() {
        nqp::isnull($!why) ?? Any !! $!why
    }
}
