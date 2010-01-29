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
    parcelproto = p6meta.'new_class'('Parcel', 'parent'=>'parrot;ResizablePMCArray Iterable')
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
    .local pmc list
    list = new ['ObjectRef'], self
    .return (list)
.end

=item perl

=cut

.namespace ['Parcel']
.sub 'perl' :method
    .local pmc self_it, perllist
    self_it = iter self
    perllist = new ['ResizablePMCArray']
  self_loop:
    unless self_it goto self_done
    $P0 = shift self_it
    $S0 = $P0.'perl'()
    push perllist, $S0
    goto self_loop
  self_done:
    $I0 = elements perllist
    unless $I0 == 1 goto have_perllist
    push perllist, ''
  have_perllist:
    $S0 = join ',', perllist
    $S0 = concat '(', $S0
    $S0 = concat $S0, ')'
    .return ($S0)
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
