## $Id$

=head1 NAME

src/classes/Abstraction.pir - Abstraction Role

=head1 DESCRIPTION

This implements the role Abstraction[].

=cut

.namespace ['Abstraction[]']

.sub '' :load :init
    # Create a parametric role with 1 possible candidate.
    .local pmc role
    .const 'Sub' $P0 = '_abstraction_role_body'
    role = new ['Perl6Role']
    $P1 = box 'Abstraction'
    setattribute role, '$!shortname', $P1
    role.'!add_variant'($P0)
    set_hll_global 'Abstraction', role
.end

.sub '_abstraction_role_body'
    .tailcall '!create_parametric_role'("Abstraction[]")
.end
.sub '' :load :init
    .local pmc block, signature
    block = get_hll_global ['Abstraction[]'], '_abstraction_role_body'
    signature = allocate_signature 0
    setprop block, "$!signature", signature
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:

