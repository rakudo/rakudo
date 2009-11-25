=begin

=head1 TITLE

Perl6::Metamodel::RoleToClassApplier

=head1 DESCRIPTION

Applies a role to a class.

=head1 METHODS

=over 4

=item apply(target, composees)

Creates a new instance of the meta-class.

=end

class Perl6::Metamodel::RoleToClassApplier;

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
        $to_compose_meta := RoleHOW.new();
        for @composees {
            RoleHOW.add_composable($to_compose_meta);
        }
        $to_compose := RoleHOW.compose($to_compose_meta);
    }

    # Collisions?
    my @collisions := RoleHOW.collisions($to_compose_meta);
    for @collisions {
        unless $target.can($target, ~$_) {
            # XXX This error is LTA.
            pir::die("Method '$_' collides and a resolution must be provided by the class");
        }
    }

    # Unsatisfied requirements?
    my @requirements := RoleHOW.requirements($to_compose_meta);
    for @requirements {
        unless $target.can($target, ~$_) {
            # XXX This error is LTA.
            pir::die("Method '$_' is required by a role and must be provided by the class, a parent class or by composing another role that implements it");
        }
    }

    # Do Parrot-level composition, which handles the methods.
    pir::addrole__vPP(pir::getattribute__PPS($target, 'parrotclass'), $to_compose);

    # XXX Attributes...

    # XXX Parents that are passed along as impl detail.
}

=begin

=back

=end
