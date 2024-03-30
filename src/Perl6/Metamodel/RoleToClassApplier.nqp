#- RoleToClassApplier ----------------------------------------------------------
my class RoleToClassApplier {
    has $!target;
    has $!to_compose;
    has @!roles;

    # Check whether it has a private method by the given name
    sub has_private_method($target, str $name) {
        nqp::existskey(
          $target.HOW.private_method_table($target), $name
        )
    }

    # Die if it does not have this private method
    sub check_private_method($target, $method) {
        has_private_method($target, $method.name)
          || unresolved_private_method($target, $method)
    }

    # Check whether it has a local method by the given name
    sub has_local_method($target, str $name) {
        nqp::existskey($target.HOW.method_table($target), $name)
          || nqp::existskey($target.HOW.submethod_table($target), $name)
    }

    # Die if it does not have this method in the class itself
    sub check_local_method($target, $method) {
        has_local_method($target, $method.name)
          || unresolved_method($target, $method)
    }

    # Die if it does not have this multi method in the given maybes
    sub check_multi_method($target, $method, @maybes) {
        my str $name  := $method.name;
        my     $multi := $method.multi;

        my int $m := nqp::elems(@maybes);
        my int $i;
        while $i < $m {
            my $maybe := nqp::atpos(@maybes, $i);
            $name eq $maybe.name
              && Perl6::Metamodel::Configuration.compare_multi_sigs(
                   $multi, $maybe.code
                 )
              ?? (return)
              !! ++$i;
        }
        unresolved_multi_method($target, $method)
    }

    # Check whether a call to a method with the given method name will resolve
    sub has_method($target, str $name) {
        my @mro  := $target.HOW.mro($target);

        my int $m := nqp::elems(@mro);
        my int $i;
        while $i < $m {
            my $class := nqp::atpos(@mro, $i);
            nqp::existskey($class.HOW.method_table($class), $name)
              || nqp::existskey($class.HOW.submethod_table($class), $name)
              ?? (return 1)
              !! ++$i;
        }
        0
    }

    # Check whether the given code object has a matching candidate
    sub has_matching_candidate($code, @candidates) {
        my int $m := nqp::elems(@candidates);
        my int $i;
        while $i < $m {
            Perl6::Metamodel::Configuration.compare_multi_sigs(
              nqp::atpos(@candidates, $i), $code
            ) ?? (return 1)
              !! ++$i;
        }
        0
    }

    method prepare($target, @roles) {
        my $targetHOW := $target.HOW;
        my $to_compose;
        my $to_composeHOW;

        # If we have many things to compose, then get them into a single helper
        # role first.
        if nqp::elems(@roles) == 1 {
            $to_compose    := nqp::atpos(@roles, 0);
            $to_composeHOW := $to_compose.HOW;
        }
        else {
            $to_compose := Perl6::Metamodel::ConcreteRoleHOW.new_type;
            $to_composeHOW := $to_compose.HOW;
            $to_composeHOW.set_language_revision(
              $to_compose, $targetHOW.language_revision
            );

            my int $m := nqp::elems(@roles);
            my int $i;
            while $i < $m {
                $to_composeHOW.add_role($to_compose, nqp::atpos(@roles, $i));
                ++$i;
            }
            $to_compose := $to_composeHOW.compose($to_compose);
        }

        # Collisions?
        my @collisions := $to_composeHOW.collisions($to_compose);
        if (my int $m := nqp::elems(@collisions)) {
            my @maybes := $targetHOW.multi_methods_to_incorporate($target);

            my int $i;
            while $i < $m {
                my $collision := nqp::atpos(@collisions, $i);
                $collision.private
                  ?? check_private_method($target, $collision)
                  !! nqp::isconcrete($collision.multi)
                    ?? check_multi_method($target, $collision, @maybes)
                    !! check_local_method($target, $collision);
                ++$i;
            }
        }

        # All preparations done, set the attributes
        $!target     := $target;
        $!to_compose := $to_compose;
        @!roles      := @roles;
    }

    method apply() {
        my $target        := $!target;
        my $targetHOW     := $target.HOW;
        my $to_compose    := $!to_compose;
        my $to_composeHOW := $to_compose.HOW;
        my @roles         := @!roles;

        # Only transfer submethods from pre-6.e roles into pre-6.e classes.
        my $with_submethods := $targetHOW.language_revision < 3
          && (nqp::not_i(nqp::istype(
               $to_composeHOW, Perl6::Metamodel::LanguageRevision
             )) || $to_composeHOW.language_revision < 3);

        my @stubs;
        my @methods      := $to_composeHOW.method_order($to_compose);
        my @method_names := $to_composeHOW.method_names($to_compose);

        # Compose in any methods.
        my int $m := nqp::elems(@methods);
        my int $i;
        while $i < $m {
            my     $method := nqp::atpos(@methods,      $i);
            my str $name   := nqp::atpos(@method_names, $i);

            # Not yet an actual method
            if nqp::can($method, 'yada') && $method.yada {
                unless has_method($target, $name)
                  || $targetHOW.has_public_attribute($target, $name) {
                    my @needed;

                    my int $n := nqp::elems(@roles);
                    my int $j;
                    while $j < $n {
                        my $role := nqp::atpos(@roles, $j);
                        nqp::push(@needed, $role.HOW.name($role))
                          if nqp::existskey(
                               $role.HOW.method_table($role), $name
                             );
                        ++$j;
                    }

                    nqp::push(
                      @stubs, nqp::hash(
                        'name',   $name,
                        'needed', @needed,
                        'target', $target,
                      )
                    );
                }
            }


            # Not a method *in* the target yet
            elsif nqp::not_i(has_local_method($target, $name))
              && ($with_submethods
                   || nqp::not_i(nqp::istype(
                        $method, Perl6::Metamodel::Configuration.submethod_type
                      ))
                 ) {
                $targetHOW.add_method($target, $name, $method);
            }

            ++$i;
        }

        # Composing into something that has private methods
        if nqp::can($to_composeHOW, 'private_method_table') {
            my @methods := $to_composeHOW.private_methods($to_compose);
            my @names   := $to_composeHOW.private_method_names($to_compose);

            my int $m := nqp::elems(@methods);
            my int $i;
            while $i < $m {
                my str $name := nqp::atpos(@names, $i);
                $targetHOW.add_private_method(
                  $target, $name, nqp::atpos(@methods, $i)
                ) unless has_private_method($target, $name);
                $i++;
            }
        }

        # Compose in any multi-methods, looking for any requirements and
        # ensuring they are met.
        if nqp::can($to_composeHOW, 'multi_methods_to_incorporate') {
            my @adds :=
              $to_composeHOW.multi_methods_to_incorporate($to_compose);

            # There are multis that need to be added
            if nqp::elems(@adds) {

                # Set up lookup of existing multis
                my @multis := $targetHOW.multi_methods_to_incorporate($target);
                my %multis;
                my int $m := nqp::elems(@multis);
                my int $i;
                while $i < $m {
                    my $multi := nqp::atpos(@multis, $i);
                    nqp::push(
                      nqp::ifnull(
                        nqp::atkey(%multis, $multi.name),
                        nqp::bindkey(%multis, $multi.name, nqp::list)
                      ),
                      $multi.code
                    );
                    ++$i;
                }

                my @yadas;
                $m := nqp::elems(@adds);
                $i := 0;
                while $i < $m {
                    my $add  := nqp::atpos(@adds, $i);
                    my $code := $add.code;

                    # Needs to be resolved
                    if nqp::can($code, 'yada') && $code.yada {
                        nqp::push(@yadas, $add);
                    }

                    # Add multi-method if no matching candidate available yet
                    else {
                        my str $name   := $add.name;

                        if nqp::islist(
                            my @candidates := nqp::atkey(%multis, $name)
                        ) {

                            # Add to target and in local lookup if no
                            # matching candidate has been found
                            unless has_matching_candidate($code, @candidates) {
                                $targetHOW.add_multi_method(
                                  $target, $name, $code
                                );
                                nqp::push(@candidates, $code);
                            }
                        }

                        # No candidate known by name yet, add it, also locally
                        else {
                            $targetHOW.add_multi_method($target, $name, $code);
                            nqp::bindkey(%multis, $name, nqp::list($code));
                        }
                    }
                    ++$i;
                }

                # Found yadas
                if ($m := nqp::elems(@yadas)) {
                    $i := 0;
                    while $i < $m {
                        my $required := nqp::atpos(@yadas, $i);

                        nqp::isnull(
                          my @candidates := nqp::atkey(%multis, $required.name)
                        ) || nqp::not_i(has_matching_candidate(
                               $required.code, @candidates
                             ))
                          ?? unimplemented_multi_method($target, $required)
                          !! ++$i;  # now has matching implementation
                    }
                }
            }
        }

        # Compose in any role attributes.
        my @attributes := $to_composeHOW.attributes($to_compose, :local);
        $m := nqp::elems(@attributes);
        $i := 0;
        while $i < $m {
            my $attribute := nqp::atpos(@attributes, $i);
            $targetHOW.has_attribute($target, $attribute.name)
              ?? attribute_exists($target, $attribute)
              !! $targetHOW.add_attribute($target, $attribute);
            ++$i;
        }

        # Compose in any parents.
        if nqp::can($to_composeHOW, 'parents') {
            my @parents := $to_composeHOW.parents($to_compose, :local);
            my int $m := nqp::elems(@parents);
            my int $i;
            while $i < $m {
                my $parent := nqp::atpos(@parents, $i);
                $targetHOW.add_parent(
                  $target,
                  $parent,
                  :hides($to_composeHOW.hides_parent($to_compose, $parent))
                );
                ++$i;
            }
        }

        # Copy any array_type.
        $targetHOW.set_array_type($target, $to_composeHOW.array_type)
          if nqp::can($targetHOW, 'is_array_type')
          && nqp::not_i($targetHOW.is_array_type)
          && nqp::can($to_composeHOW, 'is_array_type')
          && $to_composeHOW.is_array_type;

        @stubs
    }

    # Helper error subs to reduce bytecode for the fast paths
    sub unresolved_private_method($target, $method) {
        Perl6::Metamodel::Configuration.throw_or_die(
          'X::Role::Unresolved::Private',
          "Private method '"
            ~ $method.name
            ~ "' must be resolved by class "
            ~ $target.HOW.name($target)
            ~ " because it exists in multiple roles ("
            ~ nqp::join(", ", $method.roles)
            ~ ")",
            :$method,
            :$target,
        );
    }

    sub unresolved_multi_method($target, $method) {
        Perl6::Metamodel::Configuration.throw_or_die(
          'X::Role::Unresolved::Multi',
          "Multi method '"
            ~ $method.name ~ "' with signature "
            ~ $method.multi.signature.raku
            ~ " must be resolved by class "
            ~ $target.HOW.name($target)
            ~ " because it exists in multiple roles ("
            ~ nqp::join(", ", $method.roles) ~ ")",
            :$method,
            :$target
        );
    }

    sub unresolved_method($target, $method) {
        Perl6::Metamodel::Configuration.throw_or_die(
          'X::Role::Unresolved::Method',
          "Method '"
          ~ $method.name
          ~ "' must be resolved by class "
          ~ $target.HOW.name($target)
          ~ " because it exists in multiple roles ("
          ~ nqp::join(", ", $method.roles)
          ~ ")",
          :$method,
          :$target
        );
    }

    sub unimplemented_multi_method($target, $method) {
        Perl6::Metamodel::Configuration.throw_or_die(
          'X::Role::Unimplemented::Multi',
          "Multi method '"
            ~ $method.name
            ~ "' with signature "
            ~ $method.code.signature.raku
            ~ " must be implemented by "
            ~ $target.HOW.name($target)
            ~ " because it is required by a role",
          :$method,
          :$target
        );
    }

    sub attribute_exists($target, $attribute) {
        Perl6::Metamodel::Configuration.throw_or_die(
          'X::Role::Attribute::Exists',
          "Attribute '"
          ~ $attribute.name
          ~ "' already exists in the class '"
          ~ $target.HOW.name($target)
          ~ "', but a role also wishes to compose it",
          :$target,
          :$attribute
        );
    }

    Perl6::Metamodel::Configuration.set_role_to_class_applier_type(
      RoleToClassApplier
    );
}

# vim: expandtab sw=4
