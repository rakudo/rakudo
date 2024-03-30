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

        my @stubs;

        # Only transfer submethods from pre-6.e roles into pre-6.e classes.
        my $with_submethods := $targetHOW.language_revision < 3
          && (!nqp::istype($to_composeHOW, Perl6::Metamodel::LanguageRevision)
          || $to_composeHOW.language_revision < 3);

        # Compose in any methods.
        sub compose_method_table(@methods, @method_names) {
            my $method_iterator := nqp::iterator(@methods);
            for @method_names -> str $name {
                my $method := nqp::shift($method_iterator);
                if nqp::can($method, 'yada') && $method.yada {
                    unless has_method($target, $name)
                            || $targetHOW.has_public_attribute($target, $name) {
                        my @needed;
                        for @roles {
                            for nqp::hllize($_.HOW.method_table($_)) -> $m {
                                if $m.key eq $name {
                                    nqp::push(@needed, $_.HOW.name($_));
                                }
                            }
                        }
                        nqp::push(@stubs, nqp::hash('name', $name, 'needed', @needed, 'target', $target));
                    }
                }
                elsif !has_local_method($target, $name)
                        && ($with_submethods
                            || !nqp::istype($method, Perl6::Metamodel::Configuration.submethod_type))
                {
                    $targetHOW.add_method($target, $name, $method);
                }
            }
        }
        my @methods      := $to_composeHOW.method_order($to_compose);
        my @method_names := $to_composeHOW.method_names($to_compose);
        compose_method_table(
            nqp::hllize(@methods),
            nqp::hllize(@method_names),
        );
        if nqp::can($to_composeHOW, 'private_method_table') {
            my @private_methods      := nqp::hllize($to_composeHOW.private_methods($to_compose));
            my @private_method_names := nqp::hllize($to_composeHOW.private_method_names($to_compose));
            my $i := 0;
            for @private_method_names -> str $name {
                unless has_private_method($target, $name) {
                    $target.HOW.add_private_method($target, $name, @private_methods[$i]);
                }
                $i++;
            }
        }

        # Compose in any multi-methods, looking for any requirements and
        # ensuring they are met.
        if nqp::can($to_composeHOW, 'multi_methods_to_incorporate') {
            my @multis := $to_composeHOW.multi_methods_to_incorporate($to_compose);
            my @required;
            for @multis -> $add {
                my $yada := 0;
                try { $yada := $add.code.yada }
                if $yada {
                    nqp::push(@required, $add);
                }
                else {
                    my $already := 0;
                    for $targetHOW.multi_methods_to_incorporate($target) -> $existing {
                        if $existing.name eq $add.name {
                            if Perl6::Metamodel::Configuration.compare_multi_sigs($existing.code, $add.code) {
                                $already := 1;
                                last;
                            }
                        }
                    }
                    unless $already {
                        $targetHOW.add_multi_method($target, $add.name, $add.code);
                    }
                }
                for @required -> $req {
                    my $satisfaction := 0;
                    for $targetHOW.multi_methods_to_incorporate($target) -> $existing {
                        if $existing.name eq $req.name {
                            if Perl6::Metamodel::Configuration.compare_multi_sigs($existing.code, $req.code) {
                                $satisfaction := 1;
                                last;
                            }
                        }
                    }
                    unimplemented_multi_method($target, $req)
                      unless $satisfaction;
                }
            }
        }

        # Compose in any role attributes.
        my @attributes := $to_composeHOW.attributes($to_compose, :local(1));
        for @attributes {
            $targetHOW.has_attribute($target, $_.name)
              ?? attribute_exists($target, $_)
              !! $targetHOW.add_attribute($target, $_);
        }

        # Compose in any parents.
        if nqp::can($to_composeHOW, 'parents') {
            my @parents := $to_composeHOW.parents($to_compose, :local(1));
            for @parents {
                $targetHOW.add_parent($target, $_, :hides($to_composeHOW.hides_parent($to_compose, $_)));
            }
        }

        # Copy any array_type.
        if nqp::can($targetHOW, 'is_array_type') && !$targetHOW.is_array_type {
            if nqp::can($to_composeHOW, 'is_array_type') {
                if $to_composeHOW.is_array_type {
                    $targetHOW.set_array_type($target, $to_composeHOW.array_type);
                }
            }
        }

        @stubs;
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
