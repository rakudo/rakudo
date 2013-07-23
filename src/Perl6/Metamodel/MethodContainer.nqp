role Perl6::Metamodel::MethodContainer {
    # Lookup table of the methods.
    has %!methods;
    has %!submethods;

    # The order that the methods were added in.
    has @!method_order;
    
    # Cache that expires when we add methods (primarily to support NFA stuff).
    has %!cache;

    # Add a method.
    method add_method($obj, $name, $code_obj) {

        # We may get Parrot subs in here during BOOTSTRAP; the try is to cope
        # with them.
        my $method_type := "Method";
        try { $method_type := $code_obj.HOW.name($code_obj) };
        
        # Ensure we haven't already got it.
        if nqp::existskey(%!methods, $name) || nqp::existskey(%!submethods, $name) {
            nqp::die("Package '"
              ~ self.name($obj)
              ~ "' already has a "
              ~ $method_type
              ~ " '"
              ~ $name
              ~ "' (did you mean to declare a multi-method?)");
        }
        
        # Add to correct table depending on if it's a Submethod. Note, we
        if $method_type eq 'Submethod' {
            %!submethods{$name} := $code_obj;
        }
        else {
            %!methods{$name} := $code_obj;
        }

        # Adding a method means any cache is no longer authoritative.
        nqp::setmethcacheauth($obj, 0);
        %!cache := {};
        @!method_order[+@!method_order] := $code_obj;
    }

    # Gets the method hierarchy.
    method methods($obj, :$local, :$excl, :$all) {
        # Always need local methods on the list.
        my @meths;
        for @!method_order {
            @meths.push(nqp::hllizefor($_, 'perl6'));
        }

        # If local flag was not passed, include those from parents.
        unless $local {
            for self.parents($obj, :all($all), :excl($excl)) {
                for $_.HOW.method_table($_) {
                    @meths.push(nqp::hllizefor($_.value, 'perl6'));
                }
                for $_.HOW.submethod_table($_) {
                    @meths.push(nqp::hllizefor($_.value, 'perl6'));
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
    
    # Gets the submethods table.
    method submethod_table($obj) {
        %!submethods
    }
    
    # Checks if this package (not its parents) declares a given
    # method. Checks submethods also.
    method declares_method($obj, $name) {
        %!methods{$name} || %!submethods{$name} ?? 1 !! 0
    }

    # Caches or updates a cached value.
    method cache($obj, $key, $value_generator) {
        %!cache || (%!cache := {});
        nqp::existskey(%!cache, $key) ??
            %!cache{$key} !!
            (%!cache{$key} := $value_generator())
    }
}
