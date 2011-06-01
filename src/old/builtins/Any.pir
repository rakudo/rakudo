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
    .local pmc p6meta, anyproto
    p6meta = get_hll_global ['Mu'], '$!P6META'
    anyproto = p6meta.'new_class'('Any', 'parent'=>'Mu')
    set_hll_global '$!OBJECTREF', anyproto
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

=item list()

=cut

.sub 'list' :method
    $P0 = new ['List']
    $P1 = root_new ['parrot';'ResizablePMCArray']
    push $P1, self
    setattribute $P0, '@!items', $P1
    .return ($P0)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
