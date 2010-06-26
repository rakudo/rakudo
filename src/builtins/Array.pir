=head1 TITLE

Array - Perl 6 Array class

=head1 DESCRIPTION

Arrays are the mutable form of Lists.

=head2 Methods

=over 4

=cut

.namespace ['Array']
.sub 'onload' :anon :init :load
    .local pmc p6meta, proto
    p6meta = get_hll_global ['Mu'], '$!P6META'
    proto = p6meta.'new_class'('Array', 'parent'=>'List')
.end


=item new

=cut

.sub 'new' :method
    .param pmc values :slurpy
    .tailcall '&circumfix:<[ ]>'(values :flat)
.end

=back

=head2 Operators

=over 4

=item &circumfix:<[ ]>()

=cut

.namespace []
.sub '&circumfix:<[ ]>' 
    .param pmc values            :slurpy
    .local pmc parcel
    parcel = new ['Parcel']
    splice parcel, values, 0, 0
    $P0 = new ['Array']
    transform_to_p6opaque $P0
    $P0.'!STORE'(parcel)
    $P1 = new ['ObjectRef'], $P0
    $P2 = get_hll_global ['Bool'], 'True'
    setprop $P1, 'scalar', $P2
    .return ($P1)
.end

=back

=head2 Private methods

=over 4

=cut

.namespace ['Array']
.sub '!STORE' :method
    .param pmc source
    .local pmc list, flat, items, rest
    $P0 = get_hll_global 'Seq'
    list = $P0.'new'(source)
    list.'eager'()
    items = getattribute list, '@!items'
    rest  = getattribute list, '@!rest'
    if null items goto have_rest
    splice rest, items, 0, 0
    null items
  have_rest:
    flat = get_hll_global 'True'
    setattribute self, '$!flat', flat
    setattribute self, '@!items', items
    setattribute self, '@!rest', rest
    self.'eager'()
    .return (self)
.end


=item !elem(item)

Create an element for the Array (has the 'rw' property set).

=cut

.namespace ['Array']
.sub '!elem' :method
    .param pmc item
    .local pmc elem, true
    true = get_hll_global ['Bool'], 'True'
    item = descalarref item
    elem = new ['Perl6Scalar'], item
    setprop elem, 'scalar', true
    setprop elem, 'rw', true
    .return (elem)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
