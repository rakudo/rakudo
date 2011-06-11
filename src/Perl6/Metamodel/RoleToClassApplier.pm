my class RoleToClassApplier {
    sub has_method($target, $name, $local) {
        my @methods := $target.HOW.methods($target, :local($local));
        for @methods {
            if $_.name eq $name { return 1; }
        }
        return 0;
    }

    sub has_attribute($target, $name) {
        my @attributes := $target.HOW.attributes($target, :local(1));
        for @attributes {
            if $_.name eq $name { return 1; }
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
            unless has_method($target, $_.name, 1) {
                pir::die("Method '" ~ $_.name ~
                    "' must be resolved by class '" ~
                    $target.HOW.name($target) ~
                    "' because it exists in multiple roles (" ~
                    pir::join(", ", $_.roles) ~ ")");
            }
        }

        # Compose in any methods.
        my @methods := $to_compose_meta.methods($to_compose, :local(1));
        for @methods {
            unless has_method($target, $_.name, 0) {
                $target.HOW.add_method($target, $_.name, $_);
            }
        }

        # Compose in any role attributes.
        my @attributes := $to_compose_meta.attributes($to_compose, :local(1));
        for @attributes {
            if has_attribute($target, $_.name) {
                pir::die("Attribute '" ~ $_.name ~ "' already exists in the class '" ~
                    $target.HOW.name($target) ~ "', but a role also wishes to compose it");
            }
            $target.HOW.add_attribute($target, $_);
        }
        
        return $to_compose_meta.does_list($to_compose);
    }
}
