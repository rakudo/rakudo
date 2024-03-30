#- RoleToClassApplier ----------------------------------------------------------
my class RoleToClassApplier {
    has $!target;
    has $!to_compose;
    has $!to_compose_meta;
    has @!roles;

    sub has_local_method($target, str $name) {
        nqp::existskey($target.HOW.method_table($target), $name)
          || nqp::existskey($target.HOW.submethod_table($target), $name)
    }

    sub has_method($target, str $name) {
        my     @mro := $target.HOW.mro($target);
        my int $m   := nqp::elems(@mro);
        my int $i;
        while $i < $m {
            my $class := nqp::atpos(@mro, $i);
            return 1
              if nqp::existskey($class.HOW.method_table($class), $name)
              || nqp::existskey($class.HOW.submethod_table($class), $name);
            ++$i;
        }
        0
    }

    sub has_private_method($target, str $name) {
        nqp::existskey($target.HOW.private_method_table($target), $name)
    }

    method prepare($target, @roles) {
        my $HOW := $target.HOW;

        $!target := $target;
        @!roles := @roles;

        # If we have many things to compose, then get them into a single helper
        # role first.
        if nqp::elems(@roles) == 1 {
            $!to_compose := @roles[0];
            $!to_compose_meta := $!to_compose.HOW;
        }
        else {
            $!to_compose := Perl6::Metamodel::ConcreteRoleHOW.new_type;
            $!to_compose_meta := $!to_compose.HOW;
            $!to_compose_meta.set_language_revision($!to_compose, $HOW.language_revision);
            for @roles {
                $!to_compose_meta.add_role($!to_compose, $_);
            }
            $!to_compose := $!to_compose_meta.compose($!to_compose);
        }

        # Collisions?
        my @collisions := $!to_compose_meta.collisions($!to_compose);
        for @collisions {
            if $_.private {
                self.unresolved_private_method($target, $_)
                  unless has_private_method($target, $_.name);
            }
            elsif nqp::isconcrete($_.multi) {
                my $match := 0;
                for $HOW.multi_methods_to_incorporate($target) -> $maybe {
                    if $_.name eq $maybe.name &&
                            Perl6::Metamodel::Configuration.compare_multi_sigs($_.multi, $maybe.code) {
                        $match := 1;
                        last;
                    }
                }
                self.unresolved_multi_method($target, $_) unless $match;
            }
            else {
                self.unresolved_method($target, $_)
                  unless has_local_method($target, $_.name);
            }
        }
    }

    method apply() {
        my @stubs;

        # Only transfer submethods from pre-6.e roles into pre-6.e classes.
        my $with_submethods := $!target.HOW.language_revision < 3
                                && (!nqp::istype($!to_compose_meta, Perl6::Metamodel::LanguageRevision)
                                    || $!to_compose.HOW.language_revision < 3);

        # Compose in any methods.
        sub compose_method_table(@methods, @method_names) {
            my $method_iterator := nqp::iterator(@methods);
            for @method_names -> str $name {
                my $method := nqp::shift($method_iterator);
                my $yada := 0;
                try { $yada := $method.yada }
                if $yada {
                    unless has_method($!target, $name)
                            || $!target.HOW.has_public_attribute($!target, $name) {
                        my @needed;
                        for @!roles {
                            for nqp::hllize($_.HOW.method_table($_)) -> $m {
                                if $m.key eq $name {
                                    nqp::push(@needed, $_.HOW.name($_));
                                }
                            }
                        }
                        nqp::push(@stubs, nqp::hash('name', $name, 'needed', @needed, 'target', $!target));
                    }
                }
                elsif !has_local_method($!target, $name)
                        && ($with_submethods
                            || !nqp::istype($method, Perl6::Metamodel::Configuration.submethod_type))
                {
                    $!target.HOW.add_method($!target, $name, $method);
                }
            }
        }
        my @methods      := $!to_compose_meta.method_order($!to_compose);
        my @method_names := $!to_compose_meta.method_names($!to_compose);
        compose_method_table(
            nqp::hllize(@methods),
            nqp::hllize(@method_names),
        );
        if nqp::can($!to_compose_meta, 'private_method_table') {
            my @private_methods      := nqp::hllize($!to_compose_meta.private_methods($!to_compose));
            my @private_method_names := nqp::hllize($!to_compose_meta.private_method_names($!to_compose));
            my $i := 0;
            for @private_method_names -> str $name {
                unless has_private_method($!target, $name) {
                    $!target.HOW.add_private_method($!target, $name, @private_methods[$i]);
                }
                $i++;
            }
        }

        # Compose in any multi-methods, looking for any requirements and
        # ensuring they are met.
        if nqp::can($!to_compose_meta, 'multi_methods_to_incorporate') {
            my @multis := $!to_compose_meta.multi_methods_to_incorporate($!to_compose);
            my @required;
            for @multis -> $add {
                my $yada := 0;
                try { $yada := $add.code.yada }
                if $yada {
                    nqp::push(@required, $add);
                }
                else {
                    my $already := 0;
                    for $!target.HOW.multi_methods_to_incorporate($!target) -> $existing {
                        if $existing.name eq $add.name {
                            if Perl6::Metamodel::Configuration.compare_multi_sigs($existing.code, $add.code) {
                                $already := 1;
                                last;
                            }
                        }
                    }
                    unless $already {
                        $!target.HOW.add_multi_method($!target, $add.name, $add.code);
                    }
                }
                for @required -> $req {
                    my $satisfaction := 0;
                    for $!target.HOW.multi_methods_to_incorporate($!target) -> $existing {
                        if $existing.name eq $req.name {
                            if Perl6::Metamodel::Configuration.compare_multi_sigs($existing.code, $req.code) {
                                $satisfaction := 1;
                                last;
                            }
                        }
                    }
                    self.unimplemented_multi_method($!target, $req)
                      unless $satisfaction;
                }
            }
        }

        # Compose in any role attributes.
        my @attributes := $!to_compose_meta.attributes($!to_compose, :local(1));
        for @attributes {
            $!target.HOW.has_attribute($!target, $_.name)
              ?? self.attribute_exists($!target, $_)
              !! $!target.HOW.add_attribute($!target, $_);
        }

        # Compose in any parents.
        if nqp::can($!to_compose_meta, 'parents') {
            my @parents := $!to_compose_meta.parents($!to_compose, :local(1));
            for @parents {
                $!target.HOW.add_parent($!target, $_, :hides($!to_compose_meta.hides_parent($!to_compose, $_)));
            }
        }

        # Copy any array_type.
        if nqp::can($!target.HOW, 'is_array_type') && !$!target.HOW.is_array_type {
            if nqp::can($!to_compose_meta, 'is_array_type') {
                if $!to_compose_meta.is_array_type {
                    $!target.HOW.set_array_type($!target, $!to_compose_meta.array_type);
                }
            }
        }

        @stubs;
    }

    # Helper error methods to reduce bytecode for the fast paths
    method unresolved_private_method($target, $method) {
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

    method unresolved_multi_method($target, $method) { 
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

    method unresolved_method($target, $method) {
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

    method unimplemented_multi_method($target, $method) {
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

    method attribute_exists($target, $attribute) {
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

    Perl6::Metamodel::Configuration.set_role_to_class_applier_type(RoleToClassApplier);
}

# vim: expandtab sw=4
