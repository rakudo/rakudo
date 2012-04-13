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
        my %priv_meth_info;
        my %priv_meth_providers;
        for @roles {
            my $role := $_;
            sub build_meth_info(%methods, %meth_info_to_use, %meth_providers_to_use) {
                for %methods {
                    my $name := $_.key;
                    my $meth := $_.value;
                    my @meth_list;
                    my @meth_providers;
                    if pir::exists(%meth_info_to_use, $name) {
                        @meth_list := %meth_info_to_use{$name};
                        @meth_providers := %meth_providers_to_use{$name};
                    }
                    else {
                        %meth_info_to_use{$name} := @meth_list;
                        %meth_providers_to_use{$name} := @meth_providers;
                    }
                    my $found := 0;
                    for @meth_list {
                        if $meth =:= $_ {
                            $found := 1;
                        }
                        elsif pir::can($meth, 'id') && pir::can($_, 'id') {
                            $found := $meth.id == $_.id;
                        }
                    }
                    unless $found {
                        @meth_list.push($meth);
                        @meth_providers.push($role.HOW.name($role));
                    }
                }
            }
            build_meth_info($_.HOW.method_table($_), %meth_info, %meth_providers);
            build_meth_info($_.HOW.submethod_table($_), %meth_info, %meth_providers)
                if pir::can__IPs($_.HOW, 'submethod_table');
            build_meth_info($_.HOW.private_method_table($_), %priv_meth_info, %priv_meth_providers)
                if pir::can__IPs($_.HOW, 'private_method_table');
        }

        # Also need methods of target.
        my %target_meth_info := $target.HOW.method_table($target);

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
                    # Find if any of the methods are actually requirements, not
                    # implementations.
                    my @impl_meths;
                    for @add_meths {
                        my $yada := 0;
                        try { $yada := $_.yada; }
                        unless $yada {
                            @impl_meths.push($_);
                        }
                    }
                    
                    # If there's still more than one possible - add to collisions list.
                    # If we got down to just one, add it. If they were all requirements,
                    # just choose one.
                    if +@impl_meths == 1 {
                        $target.HOW.add_method($target, $name, @impl_meths[0]);
                    }
                    elsif +@impl_meths == 0 {
                        $target.HOW.add_method($target, $name, @add_meths[0]);
                    }
                    else {
                        $target.HOW.add_collision($target, $name, %meth_providers{$name});
                    }
                }
            }
        }
        
        # Process private method list.
        if pir::can__IPs($target.HOW, 'private_method_table') {
            my %target_priv_meth_info := $target.HOW.private_method_table($target);
            for %priv_meth_info {
                my $name := $_.key;
                my @add_meths := %priv_meth_info{$name};
                unless pir::exists(%target_priv_meth_info, $name) {
                    if +@add_meths == 1 {
                        $target.HOW.add_private_method($target, $name, @add_meths[0]);
                    }
                    elsif +@add_meths {
                        $target.HOW.add_collision($target, $name, %priv_meth_providers{$name}, :private(1));
                    }
                }
            }
        }

        # Now do the other bits.
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
            # caught by the multi-dispatcher later.
            if pir::can__IPs($how, 'multi_methods_to_incorporate') {
                my @multis := $how.multi_methods_to_incorporate($_);
                for @multis {
                    $target.HOW.add_multi_method($target, $_.name, $_.code);
                }
            }
            
            # Any parents can also just be copied over.
            if pir::can__IPs($how, 'parents') {
                my @parents := $how.parents($_, :local(1));
                for @parents {
                    $target.HOW.add_parent($target, $_);
                }
            }
        }

        1;
    }
}
