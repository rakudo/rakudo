#- RoleToRoleApplier -----------------------------------------------------------
my class RoleToRoleApplier {
    method apply($target, @roles) {
        my $targetHOW := $target.HOW;

        # Aggregate all of the methods sharing names, eliminating
        # any duplicates (a method can't collide with itself).
        my %method_info;
        my @method_names;
        my %method_providers;
        my %private_method_info;
        my @private_method_names;
        my %private_method_providers;
        my int $target_pre6e := $targetHOW.language_revision < 3;
        my $submethod_type := Perl6::Metamodel::Configuration.submethod_type;

        # Helper sub to return whether the given method exists in the given
        # list of methods
        sub method_exists(@methods, $target) {
            my int $m := nqp::elems(@methods);
            if nqp::can($target, 'id') {
                my int $id := $target.id;

                my int $i;
                while $i < $m {
                    my $method := nqp::atpos(@methods, $i);
                    nqp::eqaddr($method, $target)
                      || (nqp::can($method, 'id') && $method.id == $id)
                      ?? (return 1)
                      !! ++$i;
                }
            }
            else {
                my int $i;
                while $i < $m {
                    nqp::eqaddr($target, nqp::atpos(@methods, $i))
                      ?? (return 1)
                      !! ++$i;
                }
            }
            0
        }

        my int $m := nqp::elems(@roles);
        my int $i;
        while $i < $m {
            my $role    := nqp::atpos(@roles, $i);
            my $roleHOW := $role.HOW;
            my int $with_submethods := $target_pre6e
              && (nqp::can($roleHOW, 'language_revision')
                   ?? $roleHOW.language_revision < 3
                   !! 1
                 );

            sub build_method_info(
              @methods,
              @method_names,
              %method_info_to_use,
              @method_names_to_use,
              %method_providers_to_use
            ) {
                my int $m := nqp::elems(@methods);
                my int $i;
                while $i < $m {
                    my $method   := nqp::atpos(@methods, $i);

                    # Transfer all method if from pre 6.e to pre.6e, otherwise
                    # only transfer methods that are *not* submethods
                    if $with_submethods
                      || nqp::not_i(nqp::istype($method, $submethod_type)) {
                        my str $name := nqp::atpos(@method_names, $i);

                        # Use existing info if method by that name already known
                        if nqp::existskey(%method_info_to_use, $name) {
                            my @method_list :=
                              nqp::atkey(%method_info_to_use, $name);

                            # Add if method not added yet
                            unless method_exists(@method_list, $method) {
                                nqp::push(@method_list, $method);
                                nqp::push(
                                  nqp::atkey(%method_providers_to_use, $name),
                                  $roleHOW.name($role)
                                );
                            }
                        }
                        # Method not known yet, set up new info
                        else {
                            nqp::push(@method_names_to_use, $name);
                            nqp::bindkey(%method_info_to_use, $name,
                              nqp::list($method)
                            );
                            nqp::bindkey(%method_providers_to_use, $name,
                              nqp::list($roleHOW.name($role))
                            );
                        }
                    }

                    ++$i;
                }
            }

            build_method_info(
                $roleHOW.method_order($role),
                $roleHOW.method_names($role),
                %method_info,
                @method_names,
                %method_providers
            );
            build_method_info(
                $roleHOW.private_methods($role),
                $roleHOW.private_method_names($role),
                %private_method_info,
                @private_method_names,
                %private_method_providers
            ) if nqp::can($roleHOW, 'private_method_table');

            ++$i;
        }

        # Helper sub to add (private) methods to the target role that are not
        # available in the target role yet.
        sub add_methods(
          %existing,
          %info,
          @names,
          %providers,
          $adder,
        ) {
            my int $m := nqp::elems(@names);
            my int $i;
            while $i < $m {
                my str $name := nqp::atpos(@names, $i);
                my     @seen := nqp::atkey(%info, $name);

                # Do we already have a (private) method of this name? If so,
                # ignore all of the (private) methods we have from elsewhere.
                unless nqp::existskey(%existing, $name) {
                    my int $n := nqp::elems(@seen);

                    # No methods in the target role. If only one, it's easy...
                    if $n == 1 {
                        $targetHOW."$adder"(
                          $target, $name, nqp::atpos(@seen, 0)
                        );
                    }

                    # Always more than one
                    else {

                        # Find if any of the methods are actually requirements, not
                        # implementations.
                        my @implemented;
                        my int $j;
                        while $j < $n {
                            my $method := nqp::atpos(@seen, $j);
                            nqp::push(@implemented, $method)
                              unless nqp::can($method, 'yada') && $method.yada;
                            ++$j;
                        }

                        # If there's still more than one possible - add to
                        # collisions list.  If we got down to just one, add it.
                        # If they were all requirements, just choose one.
                        ($n := nqp::elems(@implemented)) > 1
                          ?? $targetHOW.add_collision(
                               $target, $name, nqp::atkey(%providers, $name)
                             )
                          !! $targetHOW."$adder"(
                               $target,
                               $name,
                               nqp::atpos(($n ?? @implemented !! @seen), 0)
                             );
                    }
                }

                ++$i;
            }
        }

        # Process methods and private methods if possible, and record any
        # collisions as well
        add_methods(
          $targetHOW.method_table($target),
          %method_info,
          @method_names,
          %method_providers,
          'add_method'
        );
        add_methods(
          $targetHOW.private_method_table($target),
          %private_method_info,
          @private_method_names,
          %private_method_providers,
          'add_private_method'
        ) if nqp::can($targetHOW, 'private_method_table');

        # Helper class to abstract information / logic about a multi candidate
        my class Candidate {
            has $!role;
            has $!code;

            method new($role, $code) {
                my $obj := nqp::create(self);
                nqp::bindattr($obj, Candidate, '$!role', $role);
                nqp::bindattr($obj, Candidate, '$!code', $code);
                $obj
            }

            method role() { $!role                  }
            method code() { $!code                  }
            method name() { $!role.HOW.name($!role) }

            # Helper method to check for (signature) collisions.  Returns
            # a list of 2 roles with a collision, or nqp::null if no
            # collisions were detected.  Takes the index to start checking
            # from to prevent a <-> b  b <-> a checks.
            method collisions(@candidates, int $i) {

                if (my int $m := nqp::elems(@candidates)) {
                    my $code := $!code;

                    # initial index is same as invocant, so skip it to prevent
                    # a false positive in collision detection
                    ++$i;

                    while $i < $m {
                        my $other := nqp::atpos(@candidates, $i);

                        nqp::not_i(nqp::eqaddr($code, $other.code))
                          && Perl6::Metamodel::Configuration.compare_multi_sigs(
                               $code, $other.code
                             )
                          ?? (return nqp::list(self.name, $other.name))
                          !! ++$i;
                    }
                }

                nqp::null
            }

            # Push the invocant on the given list if there is no instance
            # on it with the same code object.  Returns 1 if the instance
            # was added as the first entry, 0 otherwise
            method push_if_unique(@existing) {
                if (my int $m := nqp::elems(@existing)) {
                    my $code := $!code;
                    my int $i;

                    while $i < $m {
                        nqp::eqaddr($code, nqp::atpos(@existing, $i).code)
                          ?? (return 0)
                          !! ++$i;
                    }
                }

                # Empty or not found, so add and return indicating so
                nqp::push(@existing, self);
                nqp::elems(@existing) == 1
            }
        }

        # Compose multi-methods; need to pay attention to the signatures.
        my %multis_by_name;
        my @multi_names;
        my %multis_required_by_name;
        my @multis_required_names;

        $i := 0;
        while $i < $m {
            my $role    := nqp::atpos(@roles, $i);
            my $roleHOW := $role.HOW;
            my int $with_submethods := $target_pre6e
              && (nqp::can($roleHOW, 'language_revision')
                   ?? $roleHOW.language_revision < 3
                   !! 1
                 );

            if nqp::can($roleHOW, 'multi_methods_to_incorporate') {
                my @multis := $roleHOW.multi_methods_to_incorporate($role);

                my int $n := nqp::elems(@multis);
                my int $j;
                while $j < $n {
                    my     $multi := nqp::atpos(@multis, $j);
                    my str $name  := $multi.name;
                    my     $code  := $multi.code;

                    if $with_submethods
                      || nqp::not_i(nqp::istype($code, $submethod_type)) {

                        if nqp::can($code, 'yada') && $code.yada {
                            nqp::push(
                              nqp::ifnull(
                                nqp::atkey(%multis_required_by_name, $name),
                                nqp::bindkey(
                                  %multis_required_by_name, $name, nqp::list
                                )
                              ),
                              $code
                            );
                            nqp::push(@multis_required_names, $name);
                        }
                        else {
                            nqp::push(@multi_names, $name)
                              if Candidate.new($role, $code).push_if_unique(
                                   nqp::ifnull(
                                     nqp::atkey(%multis_by_name, $name),
                                     nqp::bindkey(
                                       %multis_by_name, $name, nqp::list
                                     )
                                   )
                                 );
                        }

                    }

                    ++$j;
                }
            }

            ++$i;
        }

        # Look for conflicts, and compose non-conflicting.
        $m := nqp::elems(@multi_names);
        $i := 0;
        while $i < $m {
            my str $name   := nqp::atpos(@multi_names, $i);
            my @candidates := nqp::atkey(%multis_by_name, $name);

            my int $n := nqp::elems(@candidates);
            my int $j;
            while $j < $n {
                my $candidate := nqp::atpos(@candidates, $j);

                nqp::isnull(
                  my @collisions := $candidate.collisions(@candidates, $j)
                ) ?? $targetHOW.add_multi_method(
                       $target, $name, $candidate.code)
                  !! $targetHOW.add_collision(
                       $target, $name, @collisions, :multi($candidate.code)
                     );

                ++$j;
            }

            ++$i;
        }

        # Pass on any unsatisfied requirements (note that we check for the
        # requirements being met when applying the summation of roles to a
        # class, so we can avoid duplicating that logic here.)
        $m := nqp::elems(@multis_required_names);
        $i := 0;
        while $i < $m {
            my str $name := nqp::atpos(@multis_required_names, $i);

            $targetHOW.add_multi_methods(
              $target, $name, nqp::atkey(%multis_required_by_name, $name)
            );

            ++$i;
        }


        my %current-attributes;
        my class AttrReg {
            has $!attribute;
            has $!from;

            # Create object *and* store it in hash of current attributes
            # by name
            method register($attribute, $from) {
                my $obj := nqp::create(self);

                nqp::bindattr($obj, AttrReg, '$!attribute', $attribute);
                nqp::bindattr($obj, AttrReg, '$!from',      $from     );

                nqp::bindkey(%current-attributes, $attribute.name, $obj)
            }

            method attribute() { $!attribute }
            method from()      { $!from      }
        }

        my @attributes := $targetHOW.attributes($target, :local);
        $m := nqp::elems(@attributes);
        $i := 0;
        while $i < $m {
            AttrReg.register(nqp::atpos(@attributes, $i), $target);
            ++$i;
        }

        # Now do the other bits.
        for @roles -> $r {
            my $roleHOW := $r.HOW;

            # Compose is any attributes, unless there's a conflict.
            my @attributes := $roleHOW.attributes($r, :local);
            for @attributes -> $add_attr {
                my $skip := 0;

                if nqp::can($add_attr, 'original') {
                    my $name := $add_attr.name;
                    if nqp::existskey(%current-attributes, $name) {
                        my $cur-attr := %current-attributes{$name}.attribute;
                        if (nqp::decont($cur-attr.original) =:= nqp::decont($add_attr.original)
                            && nqp::decont($cur-attr.type) =:= nqp::decont($add_attr.type))
                            || (nqp::decont($cur-attr) =:= nqp::decont($add_attr))
                        {
                            $skip := 1;
                        }
                        else {
                            if $cur-attr.name eq $add_attr.name {
                                Perl6::Metamodel::Configuration.throw_or_die(
                                    'X::Role::Attribute::Conflicts',
                                    "Attribute '" ~ $cur-attr.name ~ "' conflicts in role composition",
                                    :$target,
                                    :attribute($cur-attr),
                                    :from1(%current-attributes{$name}.from),
                                    :from2($r)
                                )
                            }
                        }
                    }
                }

                unless $skip {
                    $targetHOW.add_attribute($target, $add_attr);
                    AttrReg.register($add_attr, $r)
                }
            }

            # Any parents can also just be copied over.
            if nqp::can($roleHOW, 'parents') {
                my @parents := $roleHOW.parents($r, :local);
                for @parents -> $p {
                    $targetHOW.add_parent($target, $p, :hides($roleHOW.hides_parent($r, $p)));
                }
            }

            if nqp::can($targetHOW, 'is_array_type') && !$targetHOW.is_array_type {
                if nqp::can($roleHOW, 'is_array_type') {
                    if $roleHOW.is_array_type {
                        $targetHOW.set_array_type($target, $roleHOW.array_type);
                    }
                }
            }
        }
    }

    Perl6::Metamodel::Configuration.set_role_to_role_applier_type(RoleToRoleApplier);
}

# vim: expandtab sw=4
