role Perl6::Metamodel::AttributeContainer {
    # Attributes list.
    has @!attributes;
    has %!attribute_lookup;
    
    # Do we default them to rw?
    has $!attr_rw_by_default;

    # Adds an attribute.
    method add_attribute($obj, $meta_attr) {
        my $name := $meta_attr.name;
        if nqp::existskey(%!attribute_lookup, $name) {
            nqp::die("Package '" ~ self.name($obj) ~
                "' already has an attribute named '$name'");
        }
        @!attributes[+@!attributes] := $meta_attr;
        %!attribute_lookup{$name}   := $meta_attr;
    }

    # Composes all attributes.
    method compose_attributes($obj, :$compiler_services) {
        my %seen_with_accessor;
        my %meths := self.method_table($obj);
        my %orig_meths;
        for %meths {
            %orig_meths{$_.key} := 1;
        }
        for @!attributes {
            if $!attr_rw_by_default { $_.default_to_rw() }
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
    # attributes are rw by default.
    method set_rw($obj) {
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
