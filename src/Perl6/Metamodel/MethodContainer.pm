role Perl6::Metamodel::MethodContainer {
    # Lookup table of the methods.
    has %!methods;
    has %!submethods;

    # The order that the methods were added in.
    has @!method_order;

    # Add a method.
    method add_method($obj, $name, $code_obj) {
        # Ensure we haven't already got it.
        if %!methods{$name} || %!submethods{$name} {
            pir::die("Package '" ~ self.name($obj) ~ "' already has a method '" ~
                $name ~ "'; did you mean to declare a multi-method?");
        }
        
        # Add to correct table depending on if it's a Submethod. Note, we
        # may get Parrot subs in here during BOOTSTRAP; the try is to cope
        # with them.
        my $is_submethod := 0;
        try { $is_submethod := $code_obj.HOW.name($code_obj) eq 'Submethod' }
        if $is_submethod {
            %!submethods{$name} := $code_obj;
        }
        else {
            %!methods{$name} := $code_obj;
        }
        @!method_order[+@!method_order] := $code_obj;
    }

    # Gets the method hierarchy.
    method methods($obj, :$local) {
        # Always need local methods on the list.
        my @meths;
        for @!method_order {
            @meths.push($_);
        }

        # If local flag was not passed, include those from next thing
        # in MRO.
        unless $local {
            for self.parents($obj) {
                my @parent_meths := $_.HOW.methods($_, :local(1));
                for @parent_meths {
                    @meths.push($_);
                }
            }
        }
        
        # Return result list.
        @meths
    }

    # Get the method table. Only contains methods directly declared here,
    # and excludes submethods.
    method method_table($obj) {
        %!methods
    }
    
    method submethod_table($obj) {
        %!submethods
    }
}
