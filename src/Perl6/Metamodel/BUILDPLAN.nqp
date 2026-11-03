role Perl6::Metamodel::BUILDPLAN {
    has @!BUILDALLPLAN;
    has @!BUILDPLAN;

    method BUILDPLAN(   $XXX?) { @!BUILDPLAN    }
    method BUILDALLPLAN($XXX?) { @!BUILDALLPLAN }

    # Empty BUILDPLAN shared by all classes with empty BUILDPLANs
    my @EMPTY := nqp::list;

    # Mapping of primspec to HLL type names
    my @primspec2typename := nqp::list_s(
      "", "Int", "Num", "Str", "", "", "", "", "", "", "Int"
    );

    # Cache for HLL type objects we need to check against
    my $Code        := nqp::null;
    my $Positional  := nqp::null;
    my $Associative := nqp::null;

    # Creates the plan for building up the object. This works
    # out what we'll need to do up front, so we can just zip
    # through the "todo list" each time we need to make an object.
    # The plan is an array of code objects / arrays. If the element
    # is a code object, it should be called as a method with the named
    # parameters of the call to .bless.  If it is an array, then the
    # first element of each array is an "op" # representing the task
    # to perform:
    #   code = call as method (for BUILD or TWEAK)
    #
    # NOTE: Any changes here, should also be reflected in the
    #       lib/BUILDPLAN.rakumod module, to allow for easier
    #       core debugging of BUILDPLAN issues.
    #
    #    0 class name attr_name = set attribute from init hash
    #    1 class name attr_name = set a native int attribute from init hash
    #    2 class name attr_name = set a native num attribute from init hash
    #    3 class name attr_name = set a native str attribute from init hash
    #   10 class name attr_name = set a native uint attribute from init hash
    #  400 class attr_name code = call default closure if needed
    #  401 class attr_name code = call default closure if needed, int attr
    #  402 class attr_name code = call default closure if needed, num attr
    #  403 class attr_name code = call default closure if needed, str attr
    #  410 class attr_name code = call default closure if needed, uint attr
    #  800 die if a required attribute is not present
    #  900 class attr_name code = run attribute container initializer
    # 1000 class attr_name = touch/vivify attribute if part of mixin
    # 1100 same as 0, but init to nqp::list if value absent (nqp only)
    # 1200 same as 0, but init to nqp::hash if value absent (nqp only)
    # 1300 same as 0 but *bind* the received value + optional type constraint
    # 1400 same as 400 but *bind* the default value + optional type constraint
    # 1501 die if a required int attribute is 0
    # 1502 die if a required num attribute is 0e0
    # 1503 die if a required str attribute is null_s (will be '' in the future)
    # 1510 die if a required uint attribute is 0

    method create_BUILDPLAN($target) {

        # First, we'll create the build plan for just this class.
        my @plan;

        # Find number of attributes, and Type and primspec of each attribute,
        # to avoid repeated lookups (set at first iteration)
        my     @attributes     := self.attributes($target, :local);
        my int $num_attributes := nqp::elems(@attributes);
        my @names     := nqp::setelems(nqp::list_s, $num_attributes);
        my @types     := nqp::setelems(nqp::list,   $num_attributes);
        my @primspecs := nqp::setelems(nqp::list_i, $num_attributes);

        # When adding role's BUILD/TWEAK into the buildplan for pre-6.e
        # classes only roles of 6.e+ origin must be considered.
        my $only_6e_roles := nqp::can(self, 'language_revision')
          ?? self.language_revision < 3
          !! nqp::can(self, 'lang-rev-before')
            # Support legacy approach where implemented
            ?? self.lang-rev-before($target, 'e')
            # Assume the HOW being compiled against older Raku language version
            !! 1;

        # Add system method (BUILD or TWEAK) to the plan, checking in any
        # roles as well.
        my @ins_roles := nqp::null;
        sub add_system_method_to_plan(str $name) {
            @ins_roles := self.ins_roles($target, :with-submethods-only)
              if nqp::isnull(@ins_roles);
            my int $i := nqp::elems(@ins_roles);

            # Only submethods from roles 6.e and higher
            if $only_6e_roles {
                while --$i >= 0 {
                    my $role := nqp::atpos(@ins_roles, $i);

                    # Skip any non-6.e+ role if the target is pre-6.e
                    unless $role.HOW.language_revision < 3 {
                        my $submethod :=
                          nqp::atkey($role.HOW.submethod_table($role), $name);
                        nqp::push(@plan, $submethod)
                          unless nqp::isnull($submethod);
                    }
                }
            }

            # Any submethods from all roles
            else {
                while --$i >= 0 {
                    my $role := nqp::atpos(@ins_roles, $i);
                    my $submethod :=
                      nqp::atkey($role.HOW.submethod_table($role), $name);
                    nqp::push(@plan, $submethod)
                      unless nqp::isnull($submethod);
                }
            }

            # Does it have its own system method?
            my $method := self.find_method($target, $name, :no_fallback);
            if nqp::isconcrete($method) {
                nqp::push(@plan, $method);
                1
            }
        }

        # The HLL Mu.  Since we may wind up here at runtime, get Mu by
        # HLLizing a VMNull instead of looking it up through $*W
        my $Mu := nqp::hllizefor(nqp::null, 'Raku');

        # Emit any container initializers. Also build hash of attrs we
        # do not touch in any of the BUILDPLAN so we can spit out vivify
        # ops at the end.
        my %attrs_untouched;
        my int $i;
        while $i < $num_attributes {
            my $attribute := nqp::atpos(@attributes, $i);
            nqp::bindpos_s(
              @names, $i, my str $name := $attribute.name
            );
            nqp::bindpos(
              @types, $i, my $type := $attribute.type
            );
            nqp::bindpos_i(
              @primspecs, $i, my int $primspec := nqp::objprimspec($type)
            );

            # Do we haz a container initializer?
            if nqp::can($attribute, 'container_initializer')
              && nqp::isconcrete(my $ci := $attribute.container_initializer) {

                # https://github.com/rakudo/rakudo/issues/1226
                self.throw_compound_attribute_NYI($target, $attribute)
                  if nqp::can($attribute, 'build')
                  && nqp::isconcrete($attribute.build);

                nqp::push(@plan, nqp::list(900, $target, $name, $ci));
            }

            # Need to check for touchedness if not a native
            elsif $primspec == nqp::const::BIND_VAL_OBJ {
                nqp::bindkey(%attrs_untouched, $attribute.name, NQPMu);
            }
            ++$i;
        }

        # No custom BUILD. Rather than having an actual BUILD
        # in Mu, we produce ops here per attribute that may
        # need initializing.
        unless add_system_method_to_plan("BUILD") {
            $i := 0;
            while $i < $num_attributes {
                my $attribute := nqp::atpos(  @attributes, $i);

                # Attribute to be set at build time
                if $attribute.is_built {
                    my str $name     := nqp::atpos_s(@names, $i);
                    my int $primspec := nqp::atpos_i(@primspecs,  $i);
#?if js
                    $primspec := nqp::const::BIND_VAL_OBJ
                      if $primspec == 4 || $primspec == 5;
#?endif

                    # Set attribute from init hash
                    if $primspec || !$attribute.is_bound {
                        nqp::push(@plan, nqp::list(
                          $primspec, $target, $name, nqp::substr($name,2)
                        ));
                    }

                    # Needs binding
                    else {
                        my $entry := nqp::list(
                          1300, $target, $name, nqp::substr($name,2)
                        );
                        my $type := nqp::atpos(@types, $i);
                        nqp::push($entry, $type)
                          unless nqp::eqaddr($type, $Mu);
                        nqp::push(@plan, $entry);
                    }

                }
                ++$i;
            }
        }

        # Ensure that any required attributes are set
        $i := 0;
        while $i < $num_attributes {
            my $attribute := nqp::atpos(@attributes, $i);
            if nqp::can($attribute, 'required')
              && (my $required := $attribute.required) {
                my str $name     := nqp::atpos_s(@names,     $i);
                my int $primspec := nqp::atpos_i(@primspecs, $i);
                nqp::push(
                  @plan,
                  nqp::list(
                    ($primspec ?? 1500 + $primspec !! 800),
                    $target,
                    $name,
                    $required
                  )
                );
                nqp::deletekey(%attrs_untouched, $name);
            }
            ++$i;
        }

        # XXX Needs fix for RakuAST
        my $world := nqp::getlexdyn('$*W');

        # Check if there's any default values to put in place.
        $i := 0;
        while $i < $num_attributes {
            my $attribute := nqp::atpos(@attributes, $i);

            # compile check constants for correct type
            if nqp::can($attribute, 'build')
              && nqp::isconcrete(my $default := nqp::decont($attribute.build)) {
                my str $name     := nqp::atpos_s(@names,     $i);
                my     $type     := nqp::atpos(  @types,     $i);
                my int $primspec := nqp::atpos_i(@primspecs, $i);
#?if js
                $primspec := nqp::const::BIND_VAL_OBJ
                  if $primspec == 4 || $primspec == 5;
#?endif

                # Binding defaults to additional check at runtime
                my int $check-at-runtime :=
                  nqp::not_i($primspec) && $attribute.is_bound;
                my $entry := nqp::list(
                  ($check-at-runtime ?? 1400 !! 400 + $primspec),
                  $target,
                  $name,
                  $default
                );

                # Not compiling, no typechecks possible
                if nqp::isnull($world) {
                }

                # Currently compiling, so we can do typechecking now.
                elsif $world.in_unit_parse {

                    # Cannot typecheck code to be run later
                    if nqp::istype(
                      $default,
                      nqp::ifnull(
                        $Code,
                        $Code := $world.find_single_symbol('Code')
                      )
                    ) {
                    }

                    # Check native attribute
                    elsif $primspec {
                        my $destination := $world.find_single_symbol(
                          nqp::atpos_s(@primspec2typename, $primspec)
                        );
                        nqp::istype($default, $destination)
                          ?? ($check-at-runtime := 0)
                          !! self.throw_typecheck(
                               $attribute, $default, $destination
                             )
                    }

                    # Check opaque attribute
                    elsif nqp::istype($default, $type) {
                        $check-at-runtime := 0;
                    }

                    # Positionals could be checked now
                    elsif nqp::istype(
                      $type,
                      nqp::ifnull(
                        $Positional,
                        $Positional := $world.find_single_symbol('Positional')
                      )
                    ) && nqp::istype($default, $Positional.of) {
                        $check-at-runtime := 0;
                    }

                    # Associatives need to be checked at runtime
                    elsif nqp::istype(
                      $type,
                      nqp::ifnull(
                        $Associative,
                        $Associative := $world.find_single_symbol('Associative')
                      )
                    ) {
                    }

                    # Alas, something is wrong
                    else {
                        self.throw_typecheck($attribute, $default, $type);
                    }
                }

                nqp::push($entry, $type)
                  if $check-at-runtime
                  && nqp::not_i(nqp::eqaddr($type, $Mu));

                # store the action, mark as seen
                nqp::push(@plan, $entry);
                nqp::deletekey(%attrs_untouched, $name);
            }
            ++$i;
        }

        # Add vivify instructions for attributes not handled yet
        if nqp::elems(%attrs_untouched) {

            # Iterate over the array to get a consistent order
            $i := 0;
            while $i < $num_attributes {
                my str $name := nqp::atpos_s(@names, $i);
                nqp::push(@plan, nqp::list(1000, $target, $name))
                  if nqp::existskey(%attrs_untouched, $name);
                ++$i;
            }
        }

        # Handle any TWEAKs
        add_system_method_to_plan('TWEAK');

        # Something in the buildplan of this class
        if @plan || nqp::elems(self.parents($target)) > 1 {

            # Install plan for this class.
            @!BUILDPLAN := @plan;

            # Now create the full plan by getting the MRO, and working from
            # least derived to most derived, copying the plans.
            my @all_plan;
            my @mro := self.mro($target);
            my int $noops;

            my int $i := nqp::elems(@mro);
            while --$i >= 0 {
                my $class     := nqp::atpos(@mro, $i);
                my @buildplan := $class.HOW.BUILDPLAN($class);

                my int $n := nqp::elems(@buildplan);
                my int $j;
                while $j < $n {
                    my $entry := nqp::atpos(@buildplan, $j);
                    nqp::islist($entry) && nqp::atpos($entry, 0) == 1000
                      # noop in BUILDALLPLAN
                      ?? ($noops := 1)
                      !! nqp::push(@all_plan, $entry);
                    ++$j;
                }
            }

            # Same number of elems and no noops, identical, so just keep 1 copy
            @!BUILDALLPLAN := $noops
              || nqp::elems(@all_plan) != nqp::elems(@plan)
              ?? @all_plan
              !! @plan;
        }

        # BUILDPLAN of class itself is empty
        else {

            # Share the empty BUILDPLAN
            @!BUILDPLAN := @EMPTY;

            # Take the first "super"class's BUILDALLPLAN if possible
            my @mro := self.mro($target);
            @!BUILDALLPLAN := nqp::elems(@mro) > 1
              ?? nqp::atpos(@mro, 1).HOW.BUILDALLPLAN(nqp::atpos(@mro, 1))
              !! @EMPTY
        }
    }

    # constant value did not typecheck ok
    method throw_typecheck($attribute, $got, $expected) {
        my str $name  := $attribute.name;
        my $typecheck :=  # XXX needs fix for RakuAST
          $*W.find_symbol(["X","TypeCheck","Attribute","Default"]);

        if nqp::can($typecheck,'new') {
            $typecheck.new(
              :operation($attribute.is_bound ?? 'bind' !! 'assign'),
              :$name, :$got, :$expected
            ).throw;
        }

        # should only be in the setting
        else {
            nqp::die("Attribute '" ~ $name ~ "'s default does not match type");
        }
    }

    method ins_roles($target, :$with-submethods-only) {
        my @ins_roles;
        if $with-submethods-only && nqp::can(self, 'concretizations') {
            my @concretizations := self.concretizations($target, :local);
            my int $m := nqp::elems(@concretizations);
            my int $i;
            while $i < $m {
                my $concretization := nqp::atpos(@concretizations, $i);
                nqp::push(@ins_roles, $concretization)
                 if nqp::can($concretization.HOW, 'submethod_table');
                ++$i;
            }
        }
        @ins_roles
    }

    method throw_compound_attribute_NYI($target, $attribute) {
        my $NYI := $*R
            ?? $*R.setting-constant('X', 'Comp', 'NYI')
            !! $*W.find_symbol(["X","Comp","NYI"]);
        $NYI.new(
          feature    => "Defaults on compound attribute types",
          workaround => "Create/Adapt TWEAK method in class "
            ~ self.name($target)
            ~ ", e.g:\n\n    method TWEAK() \{\n        "
            ~ $attribute.name
            ~ " := (initial values) unless "
            ~ $attribute.name
            ~ ";\n    }"
        ).throw;
    }
}

# vim: expandtab sw=4
