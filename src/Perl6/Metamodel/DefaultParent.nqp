role Perl6::Metamodel::DefaultParent {
    my @default_parent_type;
    my $setting_only := 0;

    method set_default_parent_type($type, :$setting-only = 0) {
        @default_parent_type[0] := nqp::isconcrete($type) ?? $type !! $type.HOW.name($type);
        $setting_only := $setting-only;
    }

    method has_default_parent_type() {
        +@default_parent_type
    }

    method get_default_parent_type() {
        nqp::p6getlexclient( @default_parent_type[0], $setting_only );
    }
}
