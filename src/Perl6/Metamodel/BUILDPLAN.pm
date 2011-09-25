role Perl6::Metamodel::BUILDPLAN {
    has @!BUILDALLPLAN;
    has @!BUILDPLAN;
    
    # Creates the plan for building up the object. This works
    # out what we'll need to do up front, so we can just zip
    # through the "todo list" each time we need to make an object.
    # The plan is an array of arrays. The first element of each
    # nested array is an "op" representing the task to perform:
    #   0 code = call specified BUILD method
    #   1 class name attr_name = try to find initialization value
    #   2 class attr_name code = call default value closure if needed
    method create_BUILDPLAN($obj) {
        # Get MRO, then work from least derived to most derived.
        my @all_plan;
        my @plan;
        my @mro := self.mro($obj);
        my $i := +@mro;
        while $i > 0 {
            # Get current class to consider and its attrs.
            $i := $i - 1;
            my $class := @mro[$i];
            my @attrs := $class.HOW.attributes($class, :local(1));
            
            # Does it have its own BUILD?
            my $build := $class.HOW.find_method($class, 'BUILD', :no_fallback(1));
            if $build {
                # We'll call the custom one.
                my $entry := [0, $build];
                @all_plan[+@all_plan] := $entry;
                if $i == 0 {
                    @plan[+@plan] := $entry;
                }
            }
            else {
                # No custom BUILD. Rather than having an actual BUILD
                # in Mu, we produce ops here per attribute that may
                # need initializing.
                for @attrs {
                    if $_.has_accessor {
                        my $attr_name := $_.name;
                        my $name      := pir::substr__SSi($attr_name, 2);
                        my $entry     := [1, $class, $name, $attr_name];
                        @all_plan[+@all_plan] := $entry;
                        if $i == 0 {
                            @plan[+@plan] := $entry;
                        }
                    }
                }
            }
            
            # Check if there's any default values to put in place.
            for @attrs {
                if pir::can__IPs($_, 'build') {
                    my $default := $_.build;
                    if $default {
                        my $entry := [2, $class, $_.name, $default];
                        @all_plan[+@all_plan] := $entry;
                        if $i == 0 {
                            @plan[+@plan] := $entry;
                        }
                    }
                }
            }
        }
        @!BUILDPLAN := @plan;
        @!BUILDALLPLAN := @all_plan;
    }
    
    method BUILDPLAN($obj) {
        @!BUILDPLAN
    }
    
    method BUILDALLPLAN($obj) {
        @!BUILDALLPLAN
    }
}
