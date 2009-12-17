=begin

=head1 TITLE

Perl6::Metamodel::RoleToRoleApplier

=head1 DESCRIPTION

Applies roles to another role.

=head1 METHODS

=over 4

=item apply(target, composees)

Applies all of the composees to target.

=end

class Perl6::Metamodel::RoleToRoleApplier;

method apply($target, @composees) {
    # Aggregate all of the methods sharing names.
    my %meth_info;
    for @composees {
        my @methods := $_.HOW.methods($_.HOW);
        for @methods {
            my $name := $_.name;
            my $meth := $_;
            my @meth_list;
            if %meth_info{$name} {
                @meth_list := %meth_info{$name};
            }
            else {
                %meth_info{$name} := @meth_list;
            }
            my $found := 0;
            for @meth_list {
                if $meth =:= $_ {
                    $found := 1;
                }
            }
            unless $found {
                @meth_list.push($meth);
            }
        }
    }

    # Also need methods of target.
    my %target_meth_info;
    my @target_meths := $target.methods($target);
    for @target_meths {
        %target_meth_info{$_.name} := $_;
    }

    # Process method list.
    for %meth_info {
        my $name := ~$_;
        my @add_meths := %meth_info{$name};

        # Do we already have a method of this name? If so, ignore all of the
        # methods we have from elsewhere unless it's multi.
        if %target_meth_info{$name} {
            if %target_meth_info{$name}.multi {
                # Add them anyway.
                for @add_meths {
                    $target.add_method($target, $name, $_);
                }
            }
        }
        else {
            # No methods in the target role. If only one, it's easy...
            if +@add_meths == 1 {
                $target.add_method($target, $name, @add_meths[0]);
            }
            else {
                # More than one - add to collisions list.
                $target.add_collision($target, $name);
            }
        }
    }

    # Now do the other bits.
    my @all_composees;
    for @composees {
        my $how := $_.HOW;

        # Compose is any attributes, unless there's a conflict.
        my @attributes := $how.attributes($how);
        for @attributes {
            my $add_attr := $_;
            my $skip := 0;
            my @cur_attrs := $target.attributes($target, :local(1));
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
                $target.add_attribute($target, $add_attr);
            }
        }

        # Pass along any requirements.
        my @requirements := $how.requirements($how);
        for @requirements {
            $target.add_requirement($target, $_);
        }

        # Pass along any parents.
        my @parents := $how.parents($how);
        for @parents {
            $target.add_parent($target, $_);
        }

        # Build up full list.
        my @composees := $how.composees($how, :transitive(1));
        for @composees {
            @all_composees.push($_);
        }
    }

    return @all_composees;
}

=begin

=back

=end
