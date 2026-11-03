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

                # ...which contains an array per MRO entry, and one per
                # additional package that the attributes of an entry name,
                # so an attribute is keyed on the package it belongs to.
                # An attribute a role brings belongs to the class once
                # Attribute.compose has claimed it, and stays with the role
                # when it was composed before it got here, as the
                # attributes of a RakuAST node role are...
                my @mro := self.mro($target);
                my @other_packages;
                my @other_infos;
                my @other_bringers;

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
                    my int $others := nqp::elems(@other_packages);

                    my int $n := nqp::elems(@attributes);
                    my int $j;
                    while $j < $n {
                        my $attribute := nqp::atpos(@attributes, $j);

                        my $package := nqp::can($attribute, 'package')
                          ?? nqp::decont($attribute.package)
                          !! $type;
                        my @into := @attr_info;
                        unless nqp::isnull($package) || nqp::eqaddr($package, $type) {
                            my int $o := nqp::elems(@other_packages);
                            my int $k := $others;
                            while $k < $o {
                                nqp::eqaddr(nqp::atpos(@other_packages, $k), $package)
                                  ?? (last)
                                  !! ++$k;
                            }
                            if $k == $o {
                                nqp::push(@other_packages, $package);
                                nqp::push(@other_infos, nqp::list);
                                nqp::push(@other_bringers, $type);
                            }
                            @into := nqp::atpos(@other_infos, $k);
                        }

                        nqp::push(
                          @into,
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

                # ...and the entries for the additional packages, which
                # have no parents of their own. Each follows the class
                # that brought it. A role that reaches the class through
                # more than one entry gets one entry, so the class holds
                # one copy of its state, and that entry goes with the
                # least derived class that brings it, which keeps the
                # layout of that class a prefix of the layout of its
                # subclasses.
                if nqp::elems(@other_packages) {
                    my @class_info := @repr_info;
                    @repr_info := nqp::list;
                    my int $o := nqp::elems(@other_packages);
                    $i := 0;
                    while $i < $m {
                        my @type_info := nqp::atpos(@class_info, $i);
                        my $type := nqp::atpos(@type_info, 0);
                        nqp::push(@repr_info, @type_info);

                        my int $k;
                        while $k < $o {
                            if nqp::eqaddr(nqp::atpos(@other_bringers, $k), $type) {
                                my $package := nqp::atpos(@other_packages, $k);
                                my int $l := $k + 1;
                                while $l < $o {
                                    nqp::eqaddr(nqp::atpos(@other_packages, $l), $package)
                                      ?? (last)
                                      !! ++$l;
                                }
                                nqp::push(@repr_info, nqp::list(
                                  $package,
                                  nqp::atpos(@other_infos, $k),
                                  nqp::list
                                )) if $l == $o;
                            }
                            ++$k;
                        }
                        ++$i;
                    }
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
