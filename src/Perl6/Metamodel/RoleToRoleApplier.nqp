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
                    if nqp::existskey(%meth_info_to_use, $name) {
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
                        elsif nqp::can($meth, 'id') && nqp::can($_, 'id') {
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
                if nqp::can($_.HOW, 'submethod_table');
            build_meth_info($_.HOW.private_method_table($_), %priv_meth_info, %priv_meth_providers)
                if nqp::can($_.HOW, 'private_method_table');
        }

        # Also need methods of target, along with those it claims.
        my %target_meth_info := $target.HOW.method_table($target);
        my %target_claims;
        if nqp::can($target.HOW, 'claims') {
            for $target.HOW.claims($target) {
                %target_claims{$_} := 1;
            }
        }

        # Process method list.
        for %meth_info {
            # If the role claims the method name in question, then we ignore all
            # methods provided by other roles and just take it.
            my $name := $_.key;
            next if nqp::existskey(%target_claims, $name);

            # Get methods roles want to provide and filter out requirements.
            my @add_meths := %meth_info{$name};
            my @impl_meths;
            for @add_meths {
                my $yada := 0;
                try { $yada := $_.yada; }
                unless $yada {
                    @impl_meths.push($_);
                }
            }

            # If the target role has a method with the name...
            if nqp::existskey(%target_meth_info, $name) {
                # No implementations from composed roles means nothing to do,
                # as there's no conflict (we maybe satisfied a requirement).
                next unless @impl_meths;

                # Otherwise, add the target method into the add set, so we
                # can record the conflict.
                nqp::push(@impl_meths, %target_meth_info{$name});
                nqp::push(%meth_providers{$name}, $target.HOW.name($target));
            }

            # If we've only one implementation, add it.
            if +@impl_meths == 1 {
                $target.HOW.add_method($target, $name, @impl_meths[0]);
            }

            # Otherwise, we may have no implementations at all, in which case
            # pick any of the stubs.
            elsif +@impl_meths == 0 {
                $target.HOW.add_method($target, $name, @add_meths[0]);
            }

            # Otherwise a collision.
            else {
                $target.HOW.add_collision($target, $name, %meth_providers{$name});
            }
        }
        
        # Process private method list.
        # XXX Needs updates for claims-based algorithm.
        if nqp::can($target.HOW, 'private_method_table') {
            my %target_priv_meth_info := $target.HOW.private_method_table($target);
            for %priv_meth_info {
                my $name := $_.key;
                my @add_meths := %priv_meth_info{$name};
                unless nqp::existskey(%target_priv_meth_info, $name) {
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
                            nqp::die("Attribute '" ~ $_.name ~ "' conflicts in role composition");
                        }
                    }
                }
                unless $skip {
                    $target.HOW.add_attribute($target, $add_attr);
                }
            }
            
            # Any multi-methods go straight in; conflicts can be
            # caught by the multi-dispatcher later.
            if nqp::can($how, 'multi_methods_to_incorporate') {
                my @multis := $how.multi_methods_to_incorporate($_);
                for @multis {
                    $target.HOW.add_multi_method($target, $_.name, $_.code);
                }
            }
            
            # Any parents can also just be copied over.
            if nqp::can($how, 'parents') {
                my @parents := $how.parents($_, :local(1));
                for @parents {
                    $target.HOW.add_parent($target, $_);
                }
            }
        }

        1;
    }
}
