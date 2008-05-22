## $Id$

=head1 TITLE

Any - Perl 6 Any class

=head1 DESCRIPTION

This file implements the Any class.

=cut

.namespace [ 'Any' ]

.sub 'onload' :anon :init :load
    .local pmc p6meta
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    p6meta.'new_class'('Any', 'parent'=>'Perl6Object')
.end

.sub 'isa' :method
    .param pmc x
    $P0 = self.'HOW'()
    .return $P0.'isa'(x)
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
