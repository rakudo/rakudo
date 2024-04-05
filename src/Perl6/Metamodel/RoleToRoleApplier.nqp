my class RoleToRoleApplier {
    method apply($target, @roles) {
        my $targetHOW := $target.HOW;

        # Aggregate all of the methods sharing names, eliminating
        # any duplicates (a method can't collide with itself).
        my %meth_info;
        my @meth_names;
        my %meth_providers;
        my %priv_meth_info;
        my @priv_meth_names;
        my %priv_meth_providers;
        my $with_submethods := $targetHOW.language_revision < 3; # less than 6.e
        my $submethod_type := Perl6::Metamodel::Configuration.submethod_type;

        my int $m := nqp::elems(@roles);
        my int $i;
        while $i < $m {
            my $role    := nqp::atpos(@roles, $i);
            my $roleHOW := $role.HOW;

            sub build_meth_info(
              @methods,
              @meth_names,
              %meth_info_to_use,
              @meth_names_to_use,
              %meth_providers_to_use
            ) {
                my $meth_iterator := nqp::iterator(@methods);
                for @meth_names -> $name {
                    my $meth := nqp::shift($meth_iterator);
                    # Only transfer submethods from pre-6.e roles into pre-6.e classes.
                    next if nqp::istype($meth, $submethod_type)
                            && !($with_submethods
                                && $role.HOW.language_revision < 3);
                    my @meth_list;
                    my @meth_providers;
                    if nqp::existskey(%meth_info_to_use, $name) {
                        @meth_list := %meth_info_to_use{$name};
                        @meth_providers := %meth_providers_to_use{$name};
                    }
                    else {
                        %meth_info_to_use{$name} := @meth_list;
                        nqp::push(@meth_names_to_use, $name);
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

            build_meth_info(
                $roleHOW.method_order($role),
                $roleHOW.method_names($role),
                %meth_info,
                @meth_names,
                %meth_providers
            );
            build_meth_info(
                $roleHOW.private_methods($role),
                $roleHOW.private_method_names($role),
                %priv_meth_info,
                @priv_meth_names,
                %priv_meth_providers
            ) if nqp::can($roleHOW, 'private_method_table');

            ++$i;
        }

        # Also need methods of target.
        my %target_meth_info := $targetHOW.method_table($target);

        # Process method list.
        for @meth_names -> $name {
            my @add_meths := %meth_info{$name};

            # Do we already have a method of this name? If so, ignore all of the
            # methods we have from elsewhere.
            unless nqp::existskey(%target_meth_info, $name) {
                # No methods in the target role. If only one, it's easy...
                if nqp::elems(@add_meths) == 1 {
                    $targetHOW.add_method($target, $name, nqp::atpos(@add_meths, 0));
                }
                else {
                    # Find if any of the methods are actually requirements, not
                    # implementations.
                    my @impl_meths;
                    for @add_meths {
                        nqp::push(@impl_meths, $_)
                          unless nqp::can($_, 'yada') && $_.yada;
                    }

                    # If there's still more than one possible - add to collisions list.
                    # If we got down to just one, add it. If they were all requirements,
                    # just choose one.
                    if nqp::elems(@impl_meths) == 1 {
                        $targetHOW.add_method($target, $name, @impl_meths[0]);
                    }
                    elsif nqp::elems(@impl_meths) == 0 {
                        $targetHOW.add_method($target, $name, @add_meths[0]);
                    }
                    else {
                        $targetHOW.add_collision($target, $name, %meth_providers{$name});
                    }
                }
            }
        }

        # Process private method list.
        if nqp::can($targetHOW, 'private_method_table') {
            my %target_priv_meth_info := $targetHOW.private_method_table($target);
            for @priv_meth_names -> $name {
                my @add_meths := %priv_meth_info{$name};
                unless nqp::existskey(%target_priv_meth_info, $name) {
                    if nqp::elems(@add_meths) == 1 {
                        $targetHOW.add_private_method($target, $name, @add_meths[0]);
                    }
                    else {
                        # Find if any of the methods are actually requirements, not
                        # implementations.
                        my @impl_meths;
                        for @add_meths {
                            nqp::push(@impl_meths, $_)
                              unless nqp::can($_, 'yada') && $_.yada;
                        }

                        # If there's still more than one possible - add to collisions list.
                        # If we got down to just one, add it. If they were all requirements,
                        # just choose one.
                        if nqp::elems(@impl_meths) == 1 {
                            $targetHOW.add_private_method($target, $name, @impl_meths[0]);
                        }
                        elsif nqp::elems(@impl_meths) == 0 {
                            # any of the method stubs will do
                            $targetHOW.add_private_method($target, $name, @add_meths[0]);
                        }
                        else {
                            $targetHOW.add_collision($target, $name, %priv_meth_providers{$name}, :private(1));
                        }
                    }
                }
            }
        }

        # Compose multi-methods; need to pay attention to the signatures.
        my %multis_by_name;
        my @multi_names;
        my %multis_required_by_name;
        my @multis_required_names;
        for @roles -> $role {
            my $how := $role.HOW;
            if nqp::can($how, 'multi_methods_to_incorporate') {
                for $how.multi_methods_to_incorporate($role) {
                    my $name := $_.name;
                    my $to_add := $_.code;
                    next if nqp::istype($to_add, $submethod_type)
                            && !($with_submethods
                                && $role.HOW.language_revision < 3);
                    my $yada := 0;
                    try { $yada := $to_add.yada; }
                    if $yada {
                        %multis_required_by_name{$name} := []
                            unless %multis_required_by_name{$name};
                        nqp::push(%multis_required_by_name{$name}, $to_add);
                        nqp::push(@multis_required_names, $name);
                    }
                    else {
                        if %multis_by_name{$name} -> @existing {
                            # A multi-method can't conflict with itself.
                            my int $already := 0;
                            for @existing {
                                if $_[1] =:= $to_add {
                                    $already := 1;
                                    last;
                                }
                            }
                            nqp::push(@existing, [$role, $to_add]) unless $already;
                        }
                        else {
                            %multis_by_name{$name} := [[$role, $to_add],];
                            nqp::push(@multi_names, $name);
                        }
                    }
                }
            }
        }

        # Look for conflicts, and compose non-conflicting.
        for @multi_names -> $name {
            my @cands := %multis_by_name{$name};
            for @cands -> $c1 {
                my @collides;
                for @cands -> $c2 {
                    unless $c1[1] =:= $c2[1] {
                        if Perl6::Metamodel::Configuration.compare_multi_sigs($c1[1], $c2[1]) {
                            for ($c1, $c2) {
                                nqp::push(@collides, $_[0].HOW.name($_[0]));
                            }
                            last;
                        }
                    }
                }
                if @collides {
                    $targetHOW.add_collision($target, $name, @collides, :multi($c1[1]));
                }
                else {
                    $targetHOW.add_multi_method($target, $name, $c1[1]);
                }
            }
        }

        # Pass on any unsatisfied requirements (note that we check for the
        # requirements being met when applying the summation of roles to a
        # class, so we can avoid duplicating that logic here.)
        for @multis_required_names -> $name {
            for %multis_required_by_name{$name} {
                $targetHOW.add_multi_method($target, $name, $_);
            }
        }

        my %cur-attrs;

        my class AttrReg {
            has $!attr;
            has $!from;
            method attr() { $!attr }
            method from() { $!from }
        }

        sub reg-cur-attr($attr, $from) {
            %cur-attrs{$attr.name} := AttrReg.new(:$attr, :$from);
        }

        my @cur_attrs := $targetHOW.attributes($target, :local);
        for @cur_attrs {
            reg-cur-attr($_, $target);
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
                    if nqp::existskey(%cur-attrs, $name) {
                        my $cur-attr := %cur-attrs{$name}.attr;
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
                                    :from1(%cur-attrs{$name}.from),
                                    :from2($r)
                                )
                            }
                        }
                    }
                }

                unless $skip {
                    $targetHOW.add_attribute($target, $add_attr);
                    reg-cur-attr($add_attr, $r);
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
