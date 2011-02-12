role Perl6::Metamodel::MethodContainer {
    # Lookup table of the methods.
    has %!methods;

    # The order that the methods were added in.
    has @!method_order;

    # Add a method.
    method add_method($obj, $name, $code_obj) {
        if %!methods{$name} {
            pir::die("Package '" ~ self.name($obj) ~ "' already has a method '" ~
                $name ~ "'; did you mean to declare a multi-method?');
        }
        @!method_order[+@!method_order] := $code_obj;
        %!methods{$name} := $code_obj;
    }

    # Gets the method hierarchy.
    method methods($obj, :$local) {
        # Always need local methods on the list.
        my @meths;
        for @!method_order {
            @meths.push($_.value);
        }

        # If local flag was not passed, include those from next thing
        # in MRO.
        for self.parents($obj) {
            my @parent_meths := $_.HOW.methods($_, :local(1));
            for @parent_meths {
                @meths.push($_);
            }
        }

        # Return result list.
        @meths
    }

    # Get the method table. Only contains methods directly declared here.
    method method_table($obj) {
        %!methods
    }
}
