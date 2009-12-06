=begin

=head1 TITLE

Perl6::Metamodel::RoleToInstanceApplier

=head1 DESCRIPTION

Applies roles to an instance.

=head1 METHODS

=over 4

=item apply(target, composees)

Applies all of the composees to an instance.

=end

class Perl6::Metamodel::RoleToInstanceApplier;

method apply($target, @composees) {
    # Make anonymous subclass.
    my $how      := $target.HOW;
    my $subclass := $how.new;
    $how.add_parent($subclass, $target.WHAT);

    # Add all of our given composees to it.
    for @composees {
        $how.add_composable($subclass, $_);
    }

    # Complete construction of anonymous subclass and then rebless the target
    # into it. XXX This bit is a tad Parrot-specific at the moment; need to
    # better encapsulate reblessing.
    my $new_class := $how.compose($subclass);
    pir::rebless_subclass__vPP($target, $how.get_parrotclass($new_class));
}

=begin

=back

=end
