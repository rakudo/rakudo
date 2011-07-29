role Perl6::Metamodel::AttributeContainer {
    # Attributes list.
    has @!attributes;
    has %!attribute_lookup;
    
    # Do we default them to rw?
    has $!attr_rw_by_default;

    # Adds an attribute.
    method add_attribute($obj, $meta_attr) {
        my $name := $meta_attr.name;
        if pir::exists(%!attribute_lookup, $name) {
            pir::die("Package '" ~ self.name($obj) ~
                "' already has an attribute named '$name'");
        }
        @!attributes[+@!attributes] := $meta_attr;
        %!attribute_lookup{$name}   := $meta_attr;
    }
    
    # Composes all attributes.
    method compose_attributes($obj) {
        for @!attributes {
            if $!attr_rw_by_default { $_.default_to_rw() }
            $_.compose($obj);
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
        %!attribute_lookup{$name}
    }

    # Introspect attributes.
    method attributes($obj, :$local) {
        # Always add local ones.
        my @attrs;
        for @!attributes {
            @attrs.push($_);
        }

        # Also may need ones from parents.
        unless $local {
            for self.parents($obj) {
                for $_.HOW.attributes($_) {
                    @attrs.push($_);
                }
            }
        }

        @attrs
    }
}
