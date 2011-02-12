role Perl6::Metamodel::AttributeContainer {
    # Attributes list.
    has @!attributes;

    # Adds an attribute.
    method add_attribute($obj, $meta_attr) {
        my $name := $meta_attr.name;
        for @!attributes {
            if $_.name eq $name {
                pir::die("Package '" ~ self.name($obj) ~
                    "' already has an attribute named "' ~
                    $name ~ "'");
            }
        }
        @!attributes[+@!attributes] := $meta_attr;
    }

    # Introspect attributes.
    method attributes($obj, :$local) {
        # Always add local ones.
        my @attrs;
        for @!attributes {
            @attrs.push($_.value);
        }

        # Also may need ones from parents.
        unless $local {
            for self.parents($obj) {
                for $_.HOW.attributes($_, :local(1)) {
                    @attrs.push($_);
                }
            }
        }

        @attrs
    }
}
