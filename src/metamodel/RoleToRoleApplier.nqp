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
            my @meth_list;
            if %meth_info{$name} {
                @meth_list := %meth_info{$name};
            }
            else {
                %meth_info{$name} := @meth_list;
            }
            @meth_list.push($_);
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

    # XXX Attributes.

    # XXX Pass on any unsatisfied requirements.

    # Any parents our composees bring should be added to the target's parent
    # list.
    for @composees {
        my $how := $_.HOW;
        my @parents := $how.parents($how);
        for @parents {
            $target.add_parent($_);
        }
    }
}

=begin

=back

=end
