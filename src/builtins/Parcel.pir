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
    p6meta = get_hll_global ['Mu'], '$!P6META'
    parcelproto = p6meta.'new_class'('Parcel', 'parent'=>'parrot;ResizablePMCArray Any')
.end

=item iterator()

Construct an iterator for the Parcel.

=cut

.namespace ['Parcel']
.sub 'iterator' :method
    .local pmc listiter, rpa
    listiter = new ['ListIterator']
    rpa = new ['ResizablePMCArray']
    splice rpa, self, 0, 0
    setattribute listiter, '$!rpa', rpa
    .return (listiter)
.end


=item list()

Return a flattening version of self.

=cut

.namespace ['Parcel']
.sub 'list' :method
    .local pmc list, true
    list = new ['ObjectRef'], self
    true = get_hll_global ['Bool'], 'True'
    setprop list, 'flatten', true
    .return (list)
.end

=back

=head2 Functions

=over 4

=item &eager

Return a Parcel containing the eager evaluation of all items
in a list.

=cut

.namespace []
.sub '&eager'
    .param pmc args            :slurpy
    .local pmc eager, true
    eager = new ['Parcel']
    true = get_hll_global ['Bool'], 'True'
    setprop eager, 'flatten', true
    setprop eager, 'rw', true

    .local pmc listiter
    listiter = new ['ListIterator']
    setattribute listiter, '$!rpa', args
  iter_loop:
    .local pmc value
    value = listiter.'get'()
    $I0 = isa value, ['IterDone']
    if $I0 goto iter_done
    push eager, value
    goto iter_loop
  iter_done:
    .return (eager)
.end


=item &Nil

The canonical function for creating an empty Parcel.

=cut

.namespace []
.sub '&Nil'
    .param pmc args            :slurpy
    $P0 = '&infix:<,>'()
    .return ($P0)
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
    # treat parcel itself as rw (for list assignment)
    $P0 = get_hll_global ['Bool'], 'True'
    setprop parcel, 'rw', $P0
    setprop parcel, 'flatten', $P0
    .return (parcel)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
