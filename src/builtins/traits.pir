## $Id$

=head1 NAME

src/builtins/traits.pir - Perl 6 built-in traits

=head1 Functions

=over 4

=cut

.namespace []

# Handles the case where you have a trait that is a class being applied to a
# class, and nothing more specific in which case it's inheritance.
.sub 'trait_auxiliary:is' :multi(_,'Class')
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

# Applies the default trait to a sub.
.sub '' :anon :load :init
    # Eventually, should be a role defined in Perl 6 in the prelude. For now
    # this gets Parrot's multi-dispatch to go to the right place.
    $P0 = newclass 'default'
    set_hll_global "default", $P0
.end
.sub 'trait_auxiliary:is' :multi('default', 'Sub')
    .param pmc trait
    .param pmc the_sub
    $P0 = get_hll_global [ 'Bool' ], 'True'
    setprop the_sub, 'default', $P0
    say "oh yes"
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
