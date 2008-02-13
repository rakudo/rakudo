## $Id$

=head1 NAME

src/builtins/traits.pir - Perl 6 built-in traits

=head1 Functions

=over 4

=cut

.namespace

# Handles the case where you have a trait that is a class being applied to a
# class, in which case it's inheritance.
.sub 'trait_auxiliary:is' :multi('Class', 'Class')
    .param pmc parent
    .param pmc child
    addparent child, parent
.end
.sub 'trait_auxiliary:is' :multi('Perl6ProtoObject', 'Class')
    .param pmc parent
    .param pmc child
    parent = parent.HOW()
    'trait_auxiliary:is'(parent, child)
.end



=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
