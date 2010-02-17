=begin

=head1 TITLE

Perl6::Metamodel::RoleToClassApplier

=head1 DESCRIPTION

Applies roles to a class.

=head1 METHODS

=over 4

=item apply(target, composees)

Applies the composees to the class. If there is more than one, it builds up a
composite role first.

=end

class Perl6::Metamodel::RoleToClassApplier;

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

method apply($target, @composees) {
    # If we have many things to compose, then get them into a single helper
    # role first.
    my $to_compose;
    my $to_compose_meta;
    if +@composees == 1 {
        $to_compose := @composees[0];
        $to_compose_meta := $to_compose.HOW;
    }
    else {
        $to_compose := RoleHOW.new();
        $to_compose_meta := $to_compose.HOW;
        for @composees {
            $to_compose_meta.add_composable($to_compose, $_);
        }
        $to_compose := $to_compose_meta.compose($to_compose);
    }

    # Collisions?
    my @collisions := $to_compose_meta.collisions($to_compose);
    for @collisions {
        unless has_method($target, ~$_, 1) {
            # XXX This error is LTA.
            pir::die("Method '$_' collides and a resolution must be provided by the class");
        }
    }

    # Unsatisfied requirements?
    my @requirements := $to_compose_meta.requirements($to_compose);
    for @requirements {
        unless has_method($target, ~$_, 0) {
            # XXX This error is LTA.
            pir::die("Method '$_' is required by a role and must be provided by the class, a parent class or by composing another role that implements it");
        }
    }

    # Do Parrot-level composition, which handles the methods.
    pir::addrole__vPP(pir::getattribute__PPS($target.HOW, 'parrotclass'), $to_compose);

    # Compose in any role attributes.
    my @attributes := $to_compose_meta.attributes($to_compose);
    for @attributes {
        if has_attribute($target, $_.name) {
            pir::die("Attribute '" ~ $_.name ~ "' already exists in the class, but a role also wishes to compose it");
        }
        $target.HOW.add_attribute($target, $_);
    }

    # Add any parents that are passed along as impl detail.
    my @parents := $to_compose_meta.parents($to_compose);
    for @parents {
        $target.HOW.add_parent($target, $_);
    }

    # The full list of done roles is just the list of the one role we have
    # composed in.
    return $to_compose_meta.composees($to_compose, :transitive(1));
}

=begin

=back

=end
