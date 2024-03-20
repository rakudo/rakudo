#- Metamodel::DefaultParent ----------------------------------------------------
# Provide default parent logic
role Perl6::Metamodel::DefaultParent {
    my $default_parent_type := nqp::null;

    method set_default_parent_type($type) {
        $default_parent_type := $type;
    }

    method get_default_parent_type() {
        $default_parent_type
    }

    method has_default_parent_type() {
        nqp::not_i(nqp::isnull($default_parent_type))
    }
}

# vim: expandtab sw=4
