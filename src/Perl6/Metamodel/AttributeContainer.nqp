#- Perl6::Metamodel::AttributeContainer ----------------------------------------
# To be consumed by classes that can have attributes
role Perl6::Metamodel::AttributeContainer {
    # Attributes list.
    has @!attributes;
    has %!attribute_lookup;

    # Do we default them to rw?
    has int $!attr_rw_by_default;

    # Adds an attribute.
    method add_attribute($target, $attribute) {
        $attribute   := nqp::decont($attribute);
        my str $name := $attribute.name;
        nqp::die("Package '"
          ~ self.name($target)
          ~ "' already has an attribute named '$name'"
        ) if nqp::existskey(%!attribute_lookup, $name);

        $attribute.default_to_rw if $!attr_rw_by_default;

        # Make sure updates are threadsafe
        self.protect({
            my @attributes := nqp::clone(@!attributes);
            nqp::push(@attributes, $attribute);

            my %attribute_lookup := nqp::clone(%!attribute_lookup);
            nqp::bindkey(%attribute_lookup, $name, $attribute);

            # Update as atomically as possible
            @!attributes       := @attributes;
            %!attribute_lookup := %attribute_lookup;
        });

        $attribute
    }

    # Composes all attributes.
    method compose_attributes($target, :$compiler_services) {
        $target := nqp::decont($target);

        # Create a snapshot of the available methods at the start
        # of attribute composition
        my %snapshot := nqp::clone(self.method_table($target));

        my %seen_with_accessor;
        my @attributes := @!attributes;
        my int $m := nqp::elems(@attributes);
        my int $i;
        while $i < $m {
            my $attribute := nqp::atpos(@attributes, $i);
            if $attribute.has_accessor {
                my str $accessor := nqp::substr($attribute.name, 2);
                nqp::existskey(%seen_with_accessor, $accessor)
                  && nqp::not_i(nqp::existskey(%snapshot, $accessor))
                  ?? nqp::die(
                       "Two or more attributes declared that both want an accessor method '$accessor'"
                     )
                  !! nqp::bindkey(%seen_with_accessor, $accessor, 1);
            }

            $attribute.compose($target, :$compiler_services);

            ++$i;
        }
    }

    # Makes setting the type represented by the meta-object rw mean that its
    # attributes are rw by default. For cases when status is late set, like
    # with 'also is rw', fixup the previously added attributes. Note that we
    # can safely use 'default_to_rw' because it would pay respect to
    # `is readonly`
    method set_rw($XXX?) {
        my @attributes := @!attributes;
        my int $m := nqp::elems(@attributes);
        my int $i;
        while $i < $m {
            nqp::atpos(@attributes, $i).default_to_rw;
            ++$i;
        }
        $!attr_rw_by_default := 1;
    }

    # Is this type's attributes rw by default?
    method rw($XXX?) { $!attr_rw_by_default }

    # Gets the attribute meta-object for an attribute if it exists.
    # This is called by the parser so it should only return attributes
    # that are visible inside the current package.
    method get_attribute_for_usage($target, str $name) {
        nqp::ifnull(
          nqp::atkey(%!attribute_lookup, $name),
          nqp::die("No $name attribute in " ~ self.name($target))
        )
    }

    # Returns true if attribute exists locally.
    method has_attribute($XXX, $name) {
        nqp::existskey(%!attribute_lookup, $name)
    }

    # Returns true if the attribute has an accessor
    method has_public_attribute($XXX, $name) {
        nqp::not_i(nqp::isnull(
          my $attribute := nqp::atkey(%!attribute_lookup, $name)
        )) && $attribute.has_accessor
    }

    method attribute_table($XXX?) { %!attribute_lookup }

    # Introspect attributes.
    method attributes($target, :$local, :$excl, :$all) {
        my @attributes := nqp::clone(@!attributes);

        unless $local {
            my @parents := self.parents($target, :$excl, :$all);

            my int $m := nqp::elems(@parents);
            my int $i;
            while $i < $m {
                my $parent := nqp::atpos(@parents, $i);
                nqp::splice(
                  @attributes,
                  $parent.HOW.attributes($parent, :local),
                  nqp::elems(@attributes),
                  0
                );
                ++$i;
            }
        }

        @attributes
    }
}

# vim: expandtab sw=4
