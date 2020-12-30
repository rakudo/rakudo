role Perl6::Metamodel::Documenting {
    has $!why;

    method WHY() {
        nqp::isnull($!why) ?? Nil !! $!why
    }

    method set_why($why) {
        $!why := $why;
    }

    method is-implementation-detail($type) { nqp::hllboolfor(0,'Raku') }
}

# vim: expandtab sw=4
