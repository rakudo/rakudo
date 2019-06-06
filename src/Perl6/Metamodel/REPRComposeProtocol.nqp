role Perl6::Metamodel::REPRComposeProtocol {
    has $!composed_repr;

    method compose_repr($obj) {
        unless $!composed_repr {
            # Is it an array type?
            if nqp::can(self, 'is_array_type') && self.is_array_type($obj) {
                if self.attributes($obj) {
                    nqp::die("Cannot have attributes on an array representation");
                }
                nqp::composetype(nqp::decont($obj), nqp::hash('array',
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
                    nqp::push(@type_info, nqp::decont($type_obj));

                    # ...then an array of hashes per attribute...
                    my @attrs;
                    nqp::push(@type_info, @attrs);
                    my $build := $type_obj.HOW.find_method($obj, 'BUILD', :no_fallback(1));
                    my $has_build := !nqp::isnull($build) && nqp::isconcrete($build) ?? 1 !! 0;
                    for $type_obj.HOW.attributes(nqp::decont($type_obj), :local) -> $attr {
                        my %attr_info;
                        %attr_info<name> := $attr.name;
                        %attr_info<type> := $attr.type;
                        if $attr.box_target {
                            # Merely having the key serves as a "yes".
                            %attr_info<box_target> := 1;
                        }
                        if nqp::can($attr, 'auto_viv_container') {
                            # We only need lazy auto-viv of the container if we need to do
                            # attrinited on it. In other cases, we're better off creating
                            # it at object allocation time. The case we need to do the
                            # attrinited is when the attribute has a default *and* we
                            # have a BUILD submethod. Temporarily, we also force this
                            # to happen when the REPR is not P6opaque.
                            my $viv_value := $attr.auto_viv_container;
#?if moar
                            my $not_p6o := nqp::reprname($viv_value) ne 'P6opaque';
                            if $not_p6o || $has_build &&
                                    (nqp::can($attr, 'build') && $attr.build ||
                                     nqp::can($attr, 'required') && $attr.required) {
#?endif
                                %attr_info<auto_viv_container> := $viv_value;
#?if moar
                            }
                            else {
                                %attr_info<setup_prototype> := $viv_value;
                            }
#?endif
                        }
                        if $attr.positional_delegate {
                            %attr_info<positional_delegate> := 1;
                        }
                        if $attr.associative_delegate {
                            %attr_info<associative_delegate> := 1;
                        }
                        if nqp::can($attr, 'inlined') {
                            %attr_info<inlined> := $attr.inlined;
                        }
                        if nqp::can($attr, 'dimensions') {
                            %attr_info<dimensions> := $attr.dimensions;
                        }
                        nqp::push(@attrs, %attr_info);
                    }

                    # ...followed by a list of immediate parents.
                    nqp::push(@type_info, $type_obj.HOW.parents(nqp::decont($type_obj), :local));
                }

                # Compose the representation using it.
                nqp::composetype(nqp::decont($obj), nqp::hash('attribute', @repr_info));
            }

            $!composed_repr := 1;
        }
    }

    method repr_composed($obj) {
        $!composed_repr;
    }
}
