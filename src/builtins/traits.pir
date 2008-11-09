## $Id$

=head1 NAME

src/builtins/traits.pir - Perl 6 built-in traits

=head1 Functions

=over 4

=cut

.namespace []

# Handles the case where you have a trait that is a class being applied to a
# class, in which case it's inheritance.
.sub 'trait_auxiliary:is' :multi(_,Class)
    .param pmc parent
    .param pmc child
    .local pmc p6meta

    # Make sure we have a parent class.
    $I0 = isa parent, 'Class'
    if $I0 goto parent_ok
    'die'("Attempt to inherit from non-existent parent class")
  parent_ok:

    # Apply it.
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    parent = p6meta.'get_parrotclass'(parent)
    addparent child, parent
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
