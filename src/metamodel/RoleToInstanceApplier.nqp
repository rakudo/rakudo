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
    my $*SCOPE := 'anon';
    my $subclass := $target.HOW.new('');
    $subclass.HOW.add_parent($subclass, $target.WHAT);

    # Add all of our given composees to it.
    for @composees {
        $subclass.HOW.add_composable($subclass, $_);
    }

    # Complete construction of anonymous subclass and then rebless the target
    # into it.
    my $new_class := $subclass.HOW.compose($subclass);
    $new_class.CREATE();
    $target.HOW.rebless($target, $new_class);

    # Also need to call BUILD to perform any attribute initializations.
    my $builder := pir::find_method__pps($target, 'BUILD');
    my $*CLASS := $new_class;
    $builder($target);
}

=begin

=back

=end
