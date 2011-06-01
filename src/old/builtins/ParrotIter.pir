=head1 TITLE

ParrotIter - Maps Perl 6 Iterator to Parrot v-table

=head1 DESCRIPTION

There are some semantic mis-matches between Perl 6's iterator
model and Parrot's. This exposes the Parrot v-table interface
in terms of a Perl 6 List.

=head2 Methods

=over 4

=cut

.namespace ['ParrotIter']
.sub 'onload' :anon :init :load
    .local pmc p6meta, proto
    p6meta = get_hll_global ['Mu'], '$!P6META'
    proto = p6meta.'new_class'('ParrotIter', 'parent'=>'List')
.end


=item new

Creates a Parrot Iterator from an existing iterator and does the
initial lookahead.

=cut

.sub 'new' :method
    .param pmc srciter
    .local pmc parrotiter, rest
    parrotiter = new ['ParrotIter']
    rest = root_new ['parrot';'ResizablePMCArray']
    push rest, srciter
    setattribute parrotiter, '@!rest', rest
    .return (parrotiter)
.end
   
=item shift_pmc (vtable)

Returns the current item in the iteration, and updates the lookahead.

=cut

.sub '' :vtable('shift_pmc')
    .local pmc items, value
    items = self.'!fill'(1)
    null value
    unless items goto have_value
    value = shift items
  have_value:
    .return (value)
.end

=item get_boolean (vtable)

Reify one element to see if we've reached the end.

=cut

.sub '' :vtable('get_bool')
    .local pmc items
    items = self.'!fill'(1)
    $I0 = istrue items
    .return ($I0)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
