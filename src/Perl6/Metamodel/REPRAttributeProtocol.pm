role Perl6::Metamodel::REPRAttributeProtocol {
    method compose_repr($obj) {
        # Use any attribute information to produce attribute protocol
        # data. The protocol consists of an array...
        my @repr_info;
        
        # ...which contains an array per MRO entry...
        for self.mro($obj) -> $type_obj {
            my @type_info;
            nqp::push(@repr_info, @type_info);

            # ...which in turn contains the current type in the MRO...
            nqp::push(@type_info, $type_obj);
        
            # ...then an array of hashes per attribute...
            my @attrs;
            nqp::push(@type_info, @attrs);
            for $type_obj.HOW.attributes($type_obj, :local) -> $attr {
                my %attr_info;
                %attr_info<name> := $attr.name;
                %attr_info<type> := $attr.type;
                if $attr.box_target {
                    # Merely having the key serves as a "yes".
                    %attr_info<box_target> := 1;
                }
                if nqp::can($attr, 'auto_viv_container') {
                    %attr_info<auto_viv_container> := $attr.auto_viv_container;
                }
                nqp::push(@attrs, %attr_info);
            }
        
            # ...followed by a list of immediate parents.
            nqp::push(@type_info, $type_obj.HOW.parents($type_obj, :local));
        }
        
        # Compose the representation using it.
        nqp::composetype($obj, @repr_info)
    }
}
