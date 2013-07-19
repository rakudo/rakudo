my class RoleToClassApplier {
    sub has_method($target, $name, $local) {
        if $local {
            my %mt := $target.HOW.method_table($target);
            return 1 if nqp::existskey(%mt, $name);
            %mt := $target.HOW.submethod_table($target);
            return nqp::existskey(%mt, $name);
        }
        else {
            for $target.HOW.mro($target) {
                my %mt := $_.HOW.method_table($_);
                if nqp::existskey(%mt, $name) {
                    return 1;
                }
                %mt := $_.HOW.submethod_table($_);
                if nqp::existskey(%mt, $name) {
                    return 1;
                }
            }
            return 0;
        }
    }
    
    sub has_private_method($target, $name) {
        my %pmt := $target.HOW.private_method_table($target);
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
            return 1 if nqp::substr($_.name, 2) eq $name && $_.has_accessor;
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

        # Compose in any methods.
        sub compose_method_table(%methods) {
            for %methods {
                my $name := $_.key;
                my $yada := 0;
                try { $yada := $_.value.yada }
                if $yada {
                    unless has_method($target, $name, 0)
                            || has_public_attribute($target, $name) {
                        nqp::die("Method '$name' must be implemented by " ~
                        $target.HOW.name($target) ~
                        " because it is required by a role");
                    }
                }
                elsif !has_method($target, $name, 1) {
                    $target.HOW.add_method($target, $name, $_.value);
                }
            }
        }
        compose_method_table($to_compose_meta.method_table($to_compose));
        compose_method_table($to_compose_meta.submethod_table($to_compose))
            if nqp::can($to_compose_meta, 'submethod_table');
        if nqp::can($to_compose_meta, 'private_method_table') {
            for $to_compose_meta.private_method_table($to_compose) {
                unless has_private_method($target, $_.key) {
                    $target.HOW.add_private_method($target, $_.key, $_.value);
                }
            }
        }
        
        # Compose in any multi-methods; conflicts can be caught by
        # the multi-dispatcher later.
        if nqp::can($to_compose_meta, 'multi_methods_to_incorporate') {
            my @multis := $to_compose_meta.multi_methods_to_incorporate($to_compose);
            for @multis {
                $target.HOW.add_multi_method($target, $_.name, $_.code);
            }
        }

        # Compose in any role attributes.
        my @attributes := $to_compose_meta.attributes($to_compose, :local(1));
        for @attributes {
            if has_attribute($target, $_.name) {
                nqp::die("Attribute '" ~ $_.name ~ "' already exists in the class '" ~
                    $target.HOW.name($target) ~ "', but a role also wishes to compose it");
            }
            $target.HOW.add_attribute($target, $_);
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
        
        1;
    }
}
