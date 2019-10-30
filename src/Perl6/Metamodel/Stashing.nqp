role Perl6::Metamodel::Stashing {
    method add_stash($type_obj, :$dynamic) {
        my $stash_type :=  $dynamic
                            ?? Perl6::Metamodel::Configuration.dynamic_stash_type
                            !! Perl6::Metamodel::Configuration.stash_type;
        unless nqp::isnull($stash_type) {
            my $attr_type := Perl6::Metamodel::Configuration.stash_attr_type;
            my $stash := nqp::create($stash_type);
            nqp::bindattr($stash, $attr_type, '$!storage', my %symbols);
            $stash.set-longname($type_obj.HOW.name($type_obj));
            nqp::setwho($type_obj, $stash);
        }
        $type_obj
    }
}
