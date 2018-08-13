role Perl6::Metamodel::BUILDPLAN {
    has @!BUILDALLPLAN;
    has @!BUILDPLAN;

    # Empty BUILDPLAN shared by all classes with empty BUILDPLANs
    my @EMPTY := nqp::list;

    # Creates the plan for building up the object. This works
    # out what we'll need to do up front, so we can just zip
    # through the "todo list" each time we need to make an object.
    # The plan is an array of code objects / arrays. If the element
    # is a code object, it should be called as a method without any
    # further parameters.  If it is an array, then the first element
    # of each array is an "op" # representing the task to perform:
    #   code = call as method (for BUILD or TWEAK)
    #    0 class name attr_name = set attribute from init hash
    #    1 class name attr_name = set a native int attribute from init hash
    #    2 class name attr_name = set a native num attribute from init hash
    #    3 class name attr_name = set a native str attribute from init hash
    #    4 class attr_name code = call default value closure if needed
    #    5 class attr_name code = call default value closure if needed, int attr
    #    6 class attr_name code = call default value closure if needed, num attr
    #    7 class attr_name code = call default value closure if needed, str attr
    #    8 die if a required attribute is not present
    #    9 class attr_name code = run attribute container initializer
    #   10 class attr_name = touch/vivify attribute if part of mixin
    #   11 same as 0, but init to nqp::list if value absent (nqp only)
    #   12 same as 0, but init to nqp::hash if value absent (nqp only)
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

                    # GH #1226
                    if nqp::can($_, 'build') {
                        my $default := $_.build;
                        if nqp::isconcrete($default) {
                            $*W.find_symbol(["X","Comp","NYI"]).new(
                              feature =>
                                "Defaults on compound attribute types",
                              workaround =>
                                "Create/Adapt TWEAK method in class "
                                  ~ $obj.HOW.name($obj)
                                  ~ ", e.g:\n    method TWEAK(:"
                                  ~ $_.name
                                  ~ ' = (initial values)) { }'
                            ).throw;
                        }
                    }

                    nqp::push(@plan,[9, $obj, $_.name, $ci]);
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
            nqp::push(@plan,$build);
        }
        else {
            # No custom BUILD. Rather than having an actual BUILD
            # in Mu, we produce ops here per attribute that may
            # need initializing.
            for @attrs {
                if $_.has_accessor {
                    nqp::push(@plan,[
                      nqp::add_i(0,nqp::objprimspec($_.type)),
                      $obj,
                      $_.name,
                      nqp::substr($_.name, 2)
                    ]);
                }
            }
        }

        # Ensure that any required attributes are set
        for @attrs {
            if nqp::can($_, 'required') && $_.required {
                nqp::push(@plan,[8, $obj, $_.name, $_.required]);
                nqp::deletekey(%attrs_untouched, $_.name);
            }
        }

        # Check if there's any default values to put in place.
        for @attrs {
            if nqp::can($_, 'build') {
                my $default := $_.build;
                if nqp::isconcrete($default) {
                    nqp::push(@plan,[
                      nqp::add_i(4,nqp::objprimspec($_.type)),
                      $obj,
                      $_.name,
                      $default
                    ]);
                    nqp::deletekey(%attrs_untouched, $_.name);
                }
            }
        }

        # Add vivify instructions.
        for @attrs { # iterate over the array to get a consistent order
            if nqp::existskey(%attrs_untouched, $_.name) {
                nqp::push(@plan,[10, $obj, $_.name]);
            }
        }

        # Does it have a TWEAK?
        my $TWEAK := $obj.HOW.find_method($obj, 'TWEAK', :no_fallback(1));
        if !nqp::isnull($TWEAK) && $TWEAK {
            nqp::push(@plan,$TWEAK);
        }

        # Something in the buildplan of this class
        if @plan {

            # Install plan for this class.
            @!BUILDPLAN := @plan;

            # Now create the full plan by getting the MRO, and working from
            # least derived to most derived, copying the plans.
            my @all_plan;
            my @mro := self.mro($obj);
            my $i := +@mro;
            my $noops := 0;
            while $i > 0 {
                $i := $i - 1;
                my $class := @mro[$i];
                for $class.HOW.BUILDPLAN($class) {
                    if nqp::islist($_) && $_[0] == 10 {   # noop in BUILDALLPLAN
                        $noops := 1;
                    }
                    else {
                        nqp::push(@all_plan, $_);
                    }
                }
            }

            # Same number of elems and no noops, identical, so just keep 1 copy
            @!BUILDALLPLAN := $noops || +@all_plan != +@plan
              ?? @all_plan
              !! @plan
        }

        # BUILDPLAN of class itself is empty
        else {

            # Share the empty BUILDPLAN
            @!BUILDPLAN := @EMPTY;

            # Take the first "super"class's BUILDALLPLAN if possible
            my @mro := self.mro($obj);
            @!BUILDALLPLAN := +@mro > 1
              ?? @mro[1].HOW.BUILDALLPLAN(@mro[1])
              !! @EMPTY    
        }
    }

    method BUILDPLAN($obj) {
        @!BUILDPLAN
    }

    method BUILDALLPLAN($obj) {
        @!BUILDALLPLAN
    }
}
