my class RoleToClassApplier {
    sub has_method($target, $name, $local) {
        if $local {
            my %mt := nqp::hllize($target.HOW.method_table($target));
            return 1 if nqp::existskey(%mt, $name);
            %mt := nqp::hllize($target.HOW.submethod_table($target));
            return nqp::existskey(%mt, $name);
        }
        else {
            for $target.HOW.mro($target) {
                my %mt := nqp::hllize($_.HOW.method_table($_));
                if nqp::existskey(%mt, $name) {
                    return 1;
                }
                %mt := nqp::hllize($_.HOW.submethod_table($_));
                if nqp::existskey(%mt, $name) {
                    return 1;
                }
            }
            return 0;
        }
    }

    sub has_private_method($target, $name) {
        my %pmt := nqp::hllize($target.HOW.private_method_table($target));
        return nqp::existskey(%pmt, $name)
    }

    sub has_attribute($target, $name) {
        my @attributes := $target.HOW.attributes($target, :local(1));
        for @attributes {
            if $_.name eq $name { return 1; }
        }
        return 0;
    }
    sub has_public_attribute($target, $name) {
        my @attributes := $target.HOW.attributes($target, :local(1));
        for @attributes {
            return 1 if nqp::eqat($_.name, $name, 2) && $_.has_accessor;
        }
        return 0;
    }

    method apply($target, @roles) {
        # If we have many things to compose, then get them into a single helper
        # role first.
        my $to_compose;
        my $to_compose_meta;
        if +@roles == 1 {
            $to_compose := @roles[0];
            $to_compose_meta := $to_compose.HOW;
        }
        else {
            $to_compose := $concrete.new_type();
            $to_compose_meta := $to_compose.HOW;
            for @roles {
                $to_compose_meta.add_role($to_compose, $_);
            }
            $to_compose := $to_compose_meta.compose($to_compose);
        }

        # Collisions?
        my @collisions := $to_compose_meta.collisions($to_compose);
        for @collisions {
            if $_.private {
                unless has_private_method($target, $_.name) {
                    nqp::die("Private method '" ~ $_.name ~
                        "' must be resolved by class " ~
                        $target.HOW.name($target) ~
                        " because it exists in multiple roles (" ~
                        nqp::join(", ", $_.roles) ~ ")");
                }
            }
            elsif nqp::isconcrete($_.multi) {
                my $match := 0;
                for $target.HOW.multi_methods_to_incorporate($target) -> $maybe {
                    if $_.name eq $maybe.name &&
                            Perl6::Metamodel::Configuration.compare_multi_sigs($_.multi, $maybe.code) {
                        $match := 1;
                        last;
                    }
                }
                unless $match {
                    nqp::die("Multi method '" ~ $_.name ~ "' with signature " ~
                        $_.multi.signature.perl ~ " must be resolved by class " ~
                        $target.HOW.name($target) ~
                        " because it exists in multiple roles (" ~
                        nqp::join(", ", $_.roles) ~ ")");
                }
            }
            else {
                unless has_method($target, $_.name, 1) {
                    nqp::die("Method '" ~ $_.name ~
                        "' must be resolved by class " ~
                        $target.HOW.name($target) ~
                        " because it exists in multiple roles (" ~
                        nqp::join(", ", $_.roles) ~ ")");
                }
            }
        }

        my @stubs;

        # Compose in any methods.
        sub compose_method_table(@methods, @method_names) {
            my $method_iterator := nqp::iterator(@methods);
            for @method_names -> str $name {
                my $method := nqp::shift($method_iterator);
                my $yada := 0;
                try { $yada := $method.yada }
                if $yada {
                    unless has_method($target, $name, 0)
                            || has_public_attribute($target, $name) {
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
                elsif !has_method($target, $name, 1) {
                    $target.HOW.add_method($target, $name, $method);
                }
            }
        }
        my @methods      := $to_compose_meta.method_order($to_compose);
        my @method_names := $to_compose_meta.method_names($to_compose);
        compose_method_table(
            nqp::hllize(@methods),
            nqp::hllize(@method_names),
        );
        if nqp::can($to_compose_meta, 'private_method_table') {
            my @private_methods      := nqp::hllize($to_compose_meta.private_methods($to_compose));
            my @private_method_names := nqp::hllize($to_compose_meta.private_method_names($to_compose));
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
        if nqp::can($to_compose_meta, 'multi_methods_to_incorporate') {
            my @multis := $to_compose_meta.multi_methods_to_incorporate($to_compose);
            my @required;
            for @multis -> $add {
                my $yada := 0;
                try { $yada := $add.code.yada }
                if $yada {
                    nqp::push(@required, $add);
                }
                else {
                    my $already := 0;
                    for $target.HOW.multi_methods_to_incorporate($target) -> $existing {
                        if $existing.name eq $add.name {
                            if Perl6::Metamodel::Configuration.compare_multi_sigs($existing.code, $add.code) {
                                $already := 1;
                                last;
                            }
                        }
                    }
                    unless $already {
                        $target.HOW.add_multi_method($target, $add.name, $add.code);
                    }
                }
                for @required -> $req {
                    my $satisfaction := 0;
                    for $target.HOW.multi_methods_to_incorporate($target) -> $existing {
                        if $existing.name eq $req.name {
                            if Perl6::Metamodel::Configuration.compare_multi_sigs($existing.code, $req.code) {
                                $satisfaction := 1;
                                last;
                            }
                        }
                    }
                    unless $satisfaction {
                        my $name := $req.name;
                        my $sig := $req.code.signature.perl;
                        nqp::die("Multi method '$name' with signature $sig must be implemented by " ~
                            $target.HOW.name($target) ~
                            " because it is required by a role");
                    }
                }
            }
        }

        # Compose in any role attributes. We prepend them, so role attributes are
        # initialized before those in the class.
        my @attributes := $to_compose_meta.attributes($to_compose, :local(1));
        my int $i := nqp::elems(@attributes);
        while $i-- > 0 {
            my $attr := @attributes[$i];
            if has_attribute($target, $attr.name) {
                nqp::die("Attribute '" ~ $attr.name ~ "' already exists in the class '" ~
                    $target.HOW.name($target) ~ "', but a role also wishes to compose it");
            }
            $target.HOW.add_attribute($target, $attr, :prepend);
        }

        # Compose in any parents.
        if nqp::can($to_compose_meta, 'parents') {
            my @parents := $to_compose_meta.parents($to_compose, :local(1));
            for @parents {
                $target.HOW.add_parent($target, $_);
            }
        }

        # Copy any array_type.
        if nqp::can($target.HOW, 'is_array_type') && !$target.HOW.is_array_type($target) {
            if nqp::can($to_compose_meta, 'is_array_type') {
                if $to_compose_meta.is_array_type($to_compose) {
                    $target.HOW.set_array_type($target, $to_compose_meta.array_type($to_compose));
                }
            }
        }

        @stubs;
    }
}
