=head1 TITLE

Parcel - Perl 6 Parcel class

=head1 DESCRIPTION

This file implements Parcels, which holds a sequence of
elements and can be flattened into Captures or Lists.

=head2 Methods

=over 4

=cut

.namespace ['Parcel']
.sub 'onload' :anon :init :load
    .local pmc p6meta, parcelproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    parcelproto = p6meta.'new_class'('Parcel', 'parent'=>'parrot;ResizablePMCArray Any')
    p6meta.'register'('ResizablePMCArray', 'parent'=>parcelproto, 'protoobject'=>parcelproto)
.end

=item item()

Return the Parcel in item context.

=cut

.sub 'item' :method
    $I0 = elements self
    if $I0 != 1 goto return_list
    $P0 = self[0]
    $P1 = new ['ObjectRef'], $P0
    copy self, $P1
    .return (self)
  return_list:
    .tailcall self.'list'()
.end
 
=item list()

Return the Parcel in list context -- i.e., convert it
to a List.

=cut

.namespace ['Parcel']
.sub 'list' :method
    .local pmc values, list, gen
    values = clone self
    list = new ['List']
    copy self, list
    setattribute self, '$!values', values
    gen = box 0
    setattribute self, '$!gen', gen
    .return (self)
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
    splice parcel, args, 0, 0
    # mark the Parcel as a flattening object
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
    .tailcall self.'item'()
.end

=item !generate(n)

Parcels don't really need to know about laziness, they
can just return their list of elements and let the caller
take care of things.

=cut

.namespace ['Parcel']
.sub '!generate' :method
    .param int n               :optional
    .return (self)
.end

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
