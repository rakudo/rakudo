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
    #   2 class name attr_name = try to find initialization value, or set nqp::list()
    #   3 class name attr_name = try to find initialization value, or set nqp::hash()
    #   4 class attr_name code = call default value closure if needed
    #   5 class name attr_name = set a native int attribute
    #   6 class name attr_name = set a native num attribute
    #   7 class name attr_name = set a native str attribute
    #   8 class attr_name code = call default value closure if needed, int attr
    #   9 class attr_name code = call default value closure if needed, num attr
    #   10 class attr_name code = call default value closure if needed, str attr
    #   11 die if a required attribute is not present
    #   12 class attr_name code = run attribute container initializer
    #   13 class attr_name = touch/vivify attribute if part of mixin
    method create_BUILDPLAN($obj) {
        # First, we'll create the build plan for just this class.
        my @plan;
        my @attrs := $obj.HOW.attributes($obj, :local(1));

        # Emit any container initializers. Also build hash of attrs we
        # do not touch in any of the BUILDPLAN so we can spit out vivify
        # ops at the end.
        my %attrs_untouched;
        for @attrs {
            if nqp::can($_, 'container_initializer') {
                my $ci := $_.container_initializer;
                if nqp::isconcrete($ci) {
                    @plan[+@plan] := [12, $obj, $_.name, $ci];
                    next;
                }
            }
            if nqp::objprimspec($_.type) == 0 {
                %attrs_untouched{$_.name} := NQPMu;
            }
        }
        
        # Does it have its own BUILD?
        my $build := $obj.HOW.find_method($obj, 'BUILD', :no_fallback(1));
        if !nqp::isnull($build) && $build {
            # We'll call the custom one.
            @plan[+@plan] := [0, $build];
        }
        else {
            # No custom BUILD. Rather than having an actual BUILD
            # in Mu, we produce ops here per attribute that may
            # need initializing.
            for @attrs {
                if $_.has_accessor {
                    my $attr_name := $_.name;
                    my $name      := nqp::substr($attr_name, 2);
                    my $typespec  := nqp::objprimspec($_.type);
                    if $typespec {
                        @plan[+@plan] := [nqp::add_i(4, $typespec),
                                              $obj, $name, $attr_name];
                    } else {
                        @plan[+@plan] := [1, $obj, $name, $attr_name];
                    }
                }
            }
        }

        # Ensure that any required attributes are set
        for @attrs {
            if nqp::can($_, 'required') && $_.required {
                @plan[+@plan] := [11, $obj, $_.name, $_.required];
                nqp::deletekey(%attrs_untouched, $_.name);
            }
        }

        # Check if there's any default values to put in place.
        for @attrs {
            if nqp::can($_, 'build') {
                my $default := $_.build;
                if !nqp::isnull($default) && $default {
                    my $typespec := nqp::objprimspec($_.type);
                    if $typespec {
                        @plan[+@plan] := [nqp::add_i(7, $typespec), $obj, $_.name, $default];
                    }
                    else {
                        @plan[+@plan] := [4, $obj, $_.name, $default];
                    }
                    nqp::deletekey(%attrs_untouched, $_.name);
                }
            }
        }

        # Add vivify instructions.
        for %attrs_untouched {
            @plan[+@plan] := [13, $obj, $_.key];
        }

        # Does it have a POSTBUILD?
        my $postbuild := $obj.HOW.find_method($obj, 'POSTBUILD', :no_fallback(1));
        if !nqp::isnull($postbuild) && $postbuild {
            @plan[+@plan] := [0, $postbuild];
        }

        # Install plan for this class.
        @!BUILDPLAN := @plan;

        # Now create the full plan by getting the MRO, and working from
        # least derived to most derived, copying the plans.
        my @all_plan;
        my @mro := self.mro($obj);
        my $i := +@mro;
        while $i > 0 {
            $i := $i - 1;
            my $class := @mro[$i];
            for $class.HOW.BUILDPLAN($class) {
                nqp::push(@all_plan, $_);
            }
        }
        @!BUILDALLPLAN := @all_plan;
    }
    
    method BUILDPLAN($obj) {
        @!BUILDPLAN
    }
    
    method BUILDALLPLAN($obj) {
        @!BUILDALLPLAN
    }
}
