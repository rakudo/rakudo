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
    #   13 same as 0 but *bind* the received value
    #   14 same as 4 but *bind* the default value
    method create_BUILDPLAN($obj) {
        # First, we'll create the build plan for just this class.
        my @plan;
        my @attrs := $obj.HOW.attributes($obj, :local(1));
        my $consider-roles := !self.lang-rev-before($obj, 'e') && nqp::can(self, 'roles');

        # Emit any container initializers. Also build hash of attrs we
        # do not touch in any of the BUILDPLAN so we can spit out vivify
        # ops at the end.
        my %attrs_untouched;
        for @attrs {
            if nqp::can($_, 'container_initializer') {
                my $ci := $_.container_initializer;
                if nqp::isconcrete($ci) {

                    # https://github.com/rakudo/rakudo/issues/1226
                    if nqp::can($_, 'build') {
                        my $default := $_.build;
                        if nqp::isconcrete($default) {
                            $*W.find_symbol(["X","Comp","NYI"]).new(
                              feature =>
                                "Defaults on compound attribute types",
                              workaround =>
                                "Create/Adapt TWEAK method in class "
                                  ~ $obj.HOW.name($obj)
                                  ~ ", e.g:\n\n    method TWEAK() \{\n        "
                                  ~ $_.name
                                  ~ " := (initial values) unless "
                                  ~ $_.name
                                  ~ ";\n    }"
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

        sub add_from_roles($name) {
            my @ins_roles := self.ins_roles($obj, :with-submethods-only) unless +@ins_roles;
            my $i := +@ins_roles;
            while --$i >= 0 {
                my $submeth := nqp::atkey(@ins_roles[$i].HOW.submethod_table(@ins_roles[$i]), $name);
                if !nqp::isnull($submeth) {
                    nqp::push(@plan, $submeth);
                }
            }
        }

        add_from_roles('BUILD') if $consider-roles;

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
                my int $primspec := nqp::objprimspec($_.type);
#?if js
                my int $is_oversized_int := $primspec == 4 || $primspec == 5;
                $primspec := $is_oversized_int ?? 0 !! $primspec;
#?endif

                if $_.is_built {
                    nqp::push(@plan,[
                      ($primspec || !$_.is_bound ?? 0 + $primspec !! 13),
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
                my $type    := $_.type;
                my int $primspec := nqp::objprimspec($type);
#?if js
                my int $is_oversized_int := $primspec == 4 || $primspec == 5;
                $primspec := $is_oversized_int ?? 0 !! $primspec;
#?endif

                # compile check constants for correct type
                if nqp::isconcrete($default) {
                    if !nqp::isnull(nqp::getlexdyn('$*W')) && $*W.in_unit_parse {
                        # We're not currently compiling, skip typechecking for now.
                        if nqp::istype(nqp::decont($default), $*W.find_symbol(["Code"])) {
                            # cannot typecheck code to be run later
                        }
                        elsif $primspec {
                            # add typecheck on natives
                        }
                        elsif nqp::istype($default,$type) {
                            # type checks out ok
                        }
                        elsif nqp::istype($type,$*W.find_symbol(["Associative"])) {
                            # cannot do type checks on associatives
                        }
                        elsif nqp::istype(
                          $type,
                          my $Positional := $*W.find_symbol(["Positional"])
                        ) && nqp::istype($default,$Positional.of) {
                            # type of positional checks out ok
                        }
                        else {
                            self.throw_typecheck($_, $default, $type);
                        }
                    }

                    # all ok, push the action
                    nqp::push(@plan,[
                      ($primspec || !$_.is_bound ?? 4 + $primspec !! 14),
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

        add_from_roles('TWEAK') if $consider-roles;

        # Does it have a TWEAK?
        my $TWEAK := $obj.HOW.find_method($obj, 'TWEAK', :no_fallback(1));
        if !nqp::isnull($TWEAK) && $TWEAK {
            nqp::push(@plan,$TWEAK);
        }

        # Something in the buildplan of this class
        if @plan || nqp::elems(self.parents($obj)) > 1 {

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

    # constant value did not typecheck ok
    method throw_typecheck($attr, $default, $type) {
        my $typecheck := $*W.find_symbol(["X","TypeCheck","Attribute","Default"]);
        if nqp::can($typecheck,'new') {
            $typecheck.new(
              operation => $attr.is_bound ?? 'bind' !! 'assign',
              name      => $attr.name,
              got       => $default,
              expected  => $type,
            ).throw;
        }

        # should only be in the setting
        else {
            nqp::die("Attribute '" ~ $attr.name ~ "'s default does not match type");
        }
    }

    method ins_roles($obj, :$with-submethods-only = 0) {
        my @ins_roles;
        if nqp::can(self, 'concretizations') {
            for self.concretizations($obj, :local) {
                next if $with-submethods-only && !nqp::can($_.HOW, 'submethod_table');
                @ins_roles.push($_);
            }
        }
        @ins_roles
    }

    method BUILDPLAN($obj) {
        @!BUILDPLAN
    }

    method BUILDALLPLAN($obj) {
        @!BUILDALLPLAN
    }
}
