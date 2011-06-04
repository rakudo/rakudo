role Perl6::Metamodel::AttributeContainer {
    # Attributes list.
    has @!attributes;
    has %!attribute_lookup;

    # Adds an attribute.
    method add_attribute($obj, $meta_attr) {
        my $name := $meta_attr.name;
        for @!attributes {
            if $_.name eq $name {
                pir::die("Package '" ~ self.name($obj) ~
                    "' already has an attribute named '$name'");
            }
        }
        @!attributes[+@!attributes] := $meta_attr;
        %!attribute_lookup{$name}   := $meta_attr;
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
