role Perl6::Metamodel::Documenting {
    has $!why;

    method WHY() {
        nqp::isnull($!why) ?? Nil !! $!why
    }

    method set_why($why) {
        $!why := $why;
    }
}
