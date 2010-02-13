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
    proto = p6meta.'new_class'('Array', 'parent'=>'Seq')
.end

=item postcircumfix:<[ ]>(Int)

=cut

.sub 'postcircumfix:<[ ]>' :method :multi(_, ['Integer']) 
    .param int n
    if n < 0 goto err_index
    .local pmc values, elem
    $I0 = n + 1
    values = self.'!fill'($I0)
    elem = values[n]
    unless null elem goto have_elem
    .local pmc key
    key = box n
    elem = new ['Proxy']
    setattribute elem, '$!base', values
    setattribute elem, '$!key', key
    $P0 = get_hll_global ['Bool'], 'True'
    setprop elem, 'rw', $P0
  have_elem:
    .return (elem)

  err_index:
    "&die"("Cannot use negative index on arrays")
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
    $P1 = new ['Perl6Scalar'], $P0
    .return ($P1)
.end

=back

=head2 Private methods

=over 4

=item !elem(item)

Create an element for the Array (has the 'rw' property set).

=cut

.namespace ['Array']
.sub '!elem' :method
    .param pmc item
    .local pmc elem, true
    true = get_hll_global ['Bool'], 'True'
    elem = new ['Perl6Scalar']
    setprop elem, 'rw', true
    elem.'!STORE'(item)
    .return (elem)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
