role Perl6::Metamodel::DefaultParent {
    my @default_parent_type;

    method set_default_parent_type($type) {
        @default_parent_type[0] := $type;
    }

    method has_default_parent_type() {
        +@default_parent_type
    }

    method get_default_parent_type() {
        @default_parent_type[0]
    }
}

# vim: expandtab sw=4
