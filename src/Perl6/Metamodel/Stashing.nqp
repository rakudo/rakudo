role Perl6::Metamodel::Stashing {
    method add_stash($type_obj) {
        my $stash_type := Perl6::Metamodel::Configuration.stash_type;
        unless nqp::isnull($stash_type) {
            my $attr_type := Perl6::Metamodel::Configuration.stash_attr_type;
            my $stash := nqp::create($stash_type);
            nqp::bindattr($stash, $attr_type, '$!storage', my %symbols);
            nqp::setwho($type_obj, $stash);
        }
        $type_obj
    }
}
