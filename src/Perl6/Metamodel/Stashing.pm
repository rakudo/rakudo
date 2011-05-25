# XXX Little hacky because NQP doesn't let us see lexicals outside of
# code that does its runtime during compile time yet. Really should just
# have a my $stash_type and set/get from that.
role Perl6::Metamodel::Stashing {
    method set_stash_type($type) {
        pir::set_hll_global__vSP('StashType', $type);
    }
    
    method add_stash($type_obj) {
        unless pir::isnull__IP(pir::get_hll_global__Ps('StashType')) {
            my $stash_type := pir::get_hll_global__Ps('StashType');
            my $stash := pir::repr_instance_of__PP($stash_type);
            pir::setattribute__vPPsP($stash, $stash_type, '$!symbols', my %symbols);
            pir::set_who__vPP($type_obj, $stash);
        }
        $type_obj
    }
}
