=head1 TITLE

Parcel - Perl 6 Parcel class

=head1 DESCRIPTION

This file implements Parcels, which holds a sequence of
elements and can be flattened into Captures or Lists.

=head2 Private functions

=over 4

=cut

.namespace ['Parcel']
.sub 'onload' :anon :init :load
    .local pmc p6meta, parcelproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    parcelproto = p6meta.'new_class'('Parcel', 'parent'=>'parrot;ResizablePMCArray Any')
.end

.sub 'list' :method
    '!parcel_item'(self)
    .tailcall self.'list'()
.end

=item !parcel_item(parcel)

Return the Parcel in item context.

=cut

.namespace ['Parcel']
.sub '!parcel_item'
    .param pmc parcel
    $I0 = elements parcel
    if $I0 != 1 goto return_list
    $P0 = parcel[0]
    .return ($P0)
  return_list:
    .tailcall '!parcel_list'(parcel)
.end
 
=item !parcel_list(parcel)

Return the Parcel in list context -- i.e., convert it
to a List.

=cut

.namespace ['Parcel']
.sub '!parcel_list'
    .param pmc parcel
    .local pmc values, list, gen
    values = root_new ['parrot';'ResizablePMCArray']
    splice values, parcel, 0, 0
    list = new ['List']
    copy parcel, list
    setattribute parcel, '$!values', values
    gen = box 0
    setattribute parcel, '$!gen', gen
    .return (parcel)
.end
   
=back

=head2 Operators

=over 4

=item &infix:<,>

The canonical operator for creating a Parcel.

=cut

.namespace []
.sub '&infix:<,>'
    .param pmc args            :slurpy
    # Recast the arguments into a Parcel object
    .local pmc parcel
    parcel = new ['Parcel']
    transform_to_p6opaque parcel
    splice parcel, args, 0, 0
    # mark the Parcel as flattening objects
    $P0 = get_hll_global ['Bool'], 'True'
    setprop parcel, 'flatten', $P0
    .return (parcel)
.end

=back

=head2 Private methods

=over 4

=item !FETCH()

Retrieve a value for storage.

=cut

.namespace ['Parcel']
.sub '!FETCH' :method
    $P0 = '!parcel_item'(self)
    .tailcall $P0.'!FETCH'()
.end



=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
