role Perl6::Metamodel::REPRComposeProtocol {
    has $!composed_repr;
    
    method compose_repr($obj) {
        unless $!composed_repr {
            # Is it an array type?
            if nqp::can(self, 'is_array_type') && self.is_array_type($obj) {
                if self.attributes($obj) {
                    nqp::die("Cannot have attributes on an array representation");
                }
                nqp::composetype($obj, nqp::hash('array',
                    nqp::hash('type', nqp::decont(self.array_type($obj)))));
            }
            
            # Otherwise, presume it's an attribute type.
            else {
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
                        if $attr.positional_delegate {
                            %attr_info<positional_delegate> := 1;
                        }
                        if $attr.associative_delegate {
                            %attr_info<associative_delegate> := 1;
                        }
                        nqp::push(@attrs, %attr_info);
                    }
                
                    # ...followed by a list of immediate parents.
                    nqp::push(@type_info, $type_obj.HOW.parents($type_obj, :local));
                }
                
                # Compose the representation using it.
                nqp::composetype($obj, nqp::hash('attribute', @repr_info));
            }
            
            $!composed_repr := 1;
        }
    }
}
