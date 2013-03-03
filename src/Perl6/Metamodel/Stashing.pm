# XXX Little hacky because NQP doesn't let us see lexicals outside of
# code that does its runtime during compile time yet. Really should just
# have a my $stash_type and set/get from that.
role Perl6::Metamodel::Stashing {
    method set_stash_type($type, $attr_type) {
        nqp::bindcurhllsym('StashType', $type);
        nqp::bindcurhllsym('StashAttrType', $attr_type);
    }
    
    method add_stash($type_obj) {
        unless nqp::isnull(nqp::getcurhllsym('StashType')) {
            my $stash_type := nqp::getcurhllsym('StashType');
            my $attr_type := nqp::getcurhllsym('StashAttrType');
            my $stash := nqp::create($stash_type);
            nqp::bindattr($stash, $attr_type, '$!storage', my %symbols);
            nqp::setwho($type_obj, $stash);
        }
        $type_obj
    }
}
