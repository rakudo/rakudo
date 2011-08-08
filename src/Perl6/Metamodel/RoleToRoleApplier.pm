my class RoleToRoleApplier {
    method apply($target, @roles) {
        # Ensure we actually have something to appply.
        unless +@roles {
            return [];
        }
        
        # Aggregate all of the methods sharing names, eliminating
        # any duplicates (a method can't collide with itself).
        my %meth_info;
        my %meth_providers;
        for @roles {
            my $role := $_;
            my @methods := $_.HOW.methods($_, :local(1));
            for @methods {
                my $name := ~$_;
                my $meth := $_;
                my @meth_list;
                my @meth_providers;
                if pir::exists(%meth_info, $name) {
                    @meth_list := %meth_info{$name};
                    @meth_providers := %meth_providers{$name};
                }
                else {
                    %meth_info{$name} := @meth_list;
                    %meth_providers{$name} := @meth_providers;
                }
                my $found := 0;
                for @meth_list {
                    if $meth =:= $_ {
                        $found := 1;
                    }
                }
                unless $found {
                    @meth_list.push($meth);
                    @meth_providers.push($role.HOW.name($role));
                }
            }
        }

        # Also need methods of target.
        my %target_meth_info;
        my @target_meths := $target.HOW.methods($target, :local(1));
        for @target_meths {
            %target_meth_info{~$_} := $_;
        }

        # Process method list.
        for %meth_info {
            my $name := $_.key;
            my @add_meths := %meth_info{$name};

            # Do we already have a method of this name? If so, ignore all of the
            # methods we have from elsewhere.
            unless pir::exists(%target_meth_info, $name) {
                # No methods in the target role. If only one, it's easy...
                if +@add_meths == 1 {
                    $target.HOW.add_method($target, $name, @add_meths[0]);
                }
                else {
                    # More than one - add to collisions list.
                    $target.HOW.add_collision($target, $name, %meth_providers{$name});
                }
            }
        }

        # Now do the other bits.
        my @all_roles;
        for @roles {
            my $how := $_.HOW;

            # Compose is any attributes, unless there's a conflict.
            my @attributes := $how.attributes($_, :local(1));
            for @attributes {
                my $add_attr := $_;
                my $skip := 0;
                my @cur_attrs := $target.HOW.attributes($target, :local(1));
                for @cur_attrs {
                    if $_ =:= $add_attr {
                        $skip := 1;
                    }
                    else {
                        if $_.name eq $add_attr.name {
                            pir::die("Attribute '" ~ $_.name ~ "' conflicts in role composition");
                        }
                    }
                }
                unless $skip {
                    $target.HOW.add_attribute($target, $add_attr);
                }
            }
            
            # Any multi-methods go straight in; conflicts can be
            # caught my the multi-dispatcher later.
            if pir::can__IPs($how, 'multi_methods_to_incorporate') {
                my @multis := $how.multi_methods_to_incorporate($_);
                for @multis {
                    $target.HOW.add_multi_method($target, $_.name, $_.code);
                }
            }

            # Build up full list of roles that this one does.
            @all_roles.push($_);
            for $how.does_list($_) {
                @all_roles.push($_);
            }
        }

        return @all_roles;
    }
}
