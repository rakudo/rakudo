role Perl6::Metamodel::AttributeContainer {
    # Attributes list.
    has @!attributes;
    has %!attribute_lookup;

    # Do we default them to rw?
    has $!attr_rw_by_default;

    # Adds an attribute.
    method add_attribute($obj, $attr) {
        my $meta_attr := nqp::decont($attr);
        my $name := $meta_attr.name;
        if nqp::isnull(%!attribute_lookup) {
            @!attributes := nqp::list();
            %!attribute_lookup := nqp::hash();
        }
        if nqp::existskey(%!attribute_lookup, $name) {
            nqp::die("Package '" ~ self.name($obj) ~
                "' already has an attribute named '$name'");
        }
        if $!attr_rw_by_default { $meta_attr.default_to_rw() }
        @!attributes[+@!attributes] := $meta_attr;
        %!attribute_lookup{$name}   := $meta_attr;
    }

    # Composes all attributes.
    method compose_attributes($the-obj, :$compiler_services) {
        my $obj := nqp::decont($the-obj);

        my %seen_with_accessor;
        my %meths := nqp::hllize(self.method_table($obj));
        my %orig_meths;
        for %meths {
            %orig_meths{$_.key} := 1;
        }
        for @!attributes {
            if $_.has_accessor() {
                my $acc_name := nqp::substr($_.name, 2);
                nqp::die("Two or more attributes declared that both want an accessor method '$acc_name'")
                    if %seen_with_accessor{$acc_name} && !nqp::existskey(%orig_meths, $acc_name);
                %seen_with_accessor{$acc_name} := 1;
            }

            # Heuristic to pass along compiler_services only to Perl 6 MOP,
            # not to NQP one.
            nqp::isconcrete($compiler_services) && nqp::can($_, 'gist')
                ?? $_.compose($obj, :$compiler_services)
                !! $_.compose($obj)
        }
    }

    # Makes setting the type represented by the meta-object rw mean that its
    # attributes are rw by default. For cases when status is late set, like
    # with 'also is rw', fixup the previously added attributes. Note that we
    # can safely use 'default_to_rw' because it would pay respect to `is readonly`
    method set_rw($obj) {
        for @!attributes {
            $_.default_to_rw();
        }
        $!attr_rw_by_default := 1;
    }

    # Is this type's attributes rw by default?
    method rw($obj) {
        $!attr_rw_by_default
    }

    # Gets the attribute meta-object for an attribute if it exists.
    # This is called by the parser so it should only return attributes
    # that are visible inside the current package.
    method get_attribute_for_usage($obj, $name) {
        unless nqp::existskey(%!attribute_lookup, $name) {
            nqp::die("No $name attribute in " ~ self.name($obj))
        }
        %!attribute_lookup{$name}
    }

    # Returns true if attribute exists locally.
    method has_attribute($obj, $name) {
        nqp::existskey(%!attribute_lookup, $name)
    }
    method has_public_attribute($obj, $name) {
        nqp::existskey(%!attribute_lookup, $name) && %!attribute_lookup{$name}.has_accessor
    }

    method attribute_table($obj) {
        %!attribute_lookup
    }

    # Introspect attributes.
    method attributes($obj, :$local, :$excl, :$all) {
        my @attrs;

        for @!attributes {
            @attrs.push($_);
        }

        unless $local {
            for self.parents($obj, :excl($excl), :all($all)) {
                for $_.HOW.attributes($_, :local(1)) {
                    @attrs.push($_);
                }
            }
        }

        @attrs
    }
}

# vim: expandtab sw=4
