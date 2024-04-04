#- Metamodel::REPRComposeProtocol ----------------------------------------------
role Perl6::Metamodel::REPRComposeProtocol {
    has $!composed_repr;

    method compose_repr($target) {

        # Can only compose once
        unless $!composed_repr {
            my %composition;

            # Is it an array type?
            if nqp::can(self, 'is_array_type') && self.is_array_type {
                nqp::die("Cannot have attributes on an array representation")
                  if nqp::elems(self.attributes($target));

                nqp::bindkey(%composition,
                  'array', nqp::hash('type', nqp::decont(self.array_type))
                );
            }

            # Otherwise, presume it's an attribute type.
            else {

                # Use any attribute information to produce attribute protocol
                # data. The protocol consists of an array...
                my @repr_info;

                # ...which contains an array per MRO entry...
                my @mro := self.mro($target);

                my int $m := nqp::elems(@mro);
                my int $i;
                while $i < $m {
                    my $type       := nqp::decont(nqp::atpos(@mro, $i));
                    my @attributes := $type.HOW.attributes($type, :local);

                    # ...which in turn contains the current type in the MRO...
                    nqp::push(
                      @repr_info,
                      (my @type_info := nqp::list($type))
                    );

                    # ...then an array of hashes per attribute...
                    my @attr_info;
                    nqp::push(@type_info, @attr_info);

                    my int $n := nqp::elems(@attributes);
                    my int $j;
                    while $j < $n {
                        my $attribute := nqp::atpos(@attributes, $j);

                        nqp::push(
                          @attr_info,
                          (my %info := nqp::hash(
                            'name', $attribute.name,
                            'type', $attribute.type
                          ))
                        );

                        # Merely having the key serves as a "yes".
                        nqp::bindkey(%info, 'box_target', 1)
                          if $attribute.box_target;

                        nqp::bindkey(%info,
                          'auto_viv_container', $attribute.auto_viv_container
                        ) if nqp::can($attribute, 'auto_viv_container');

                        nqp::bindkey(%info, 'positional_delegate', 1)
                          if $attribute.positional_delegate;

                        nqp::bindkey(%info, 'associative_delegate', 1)
                          if $attribute.associative_delegate;

                        nqp::bindkey(%info, 'inlined', $attribute.inlined)
                          if nqp::can($attribute, 'inlined');

                        nqp::bindkey(%info, 'dimensions', $attribute.dimensions)
                          if nqp::can($attribute, 'dimensions');

                        ++$j;
                    }

                    # ...followed by a list of immediate parents.
                    nqp::push(@type_info, $type.HOW.parents($type, :local));

                    ++$i;
                }

                nqp::bindkey(%composition, 'attribute', @repr_info);
            }

            # Compose the representation using it.
            nqp::composetype(nqp::decont($target), %composition);
            $!composed_repr := 1;
        }
    }

    method repr_composed($XXX?) { $!composed_repr }
}

# vim: expandtab sw=4
