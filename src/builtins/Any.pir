## $Id$

=head1 TITLE

Any - Perl 6 Any class

=head1 DESCRIPTION

This file implements the Any class.

=head2 Basic C<Any> methods

=over 4

=cut

.namespace []
.sub 'onload' :anon :init :load
    .local pmc p6meta
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    p6meta.'new_class'('Any', 'parent'=>'Perl6Object')
.end


=item can($x)

=cut

.namespace ['Any']
.sub 'can' :method
    .param pmc x
    $P0 = self.'HOW'()
    .tailcall $P0.'can'(self, x)
.end

=item isa($x)

=cut

.sub 'isa' :method
    .param pmc x
    $P0 = self.'HOW'()
    .tailcall $P0.'isa'(self, x)
.end

=item does($x)

=cut

.sub 'does' :method
    .param pmc x
    $P0 = self.'HOW'()
    .tailcall $P0.'does'(self, x)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
