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

.sub 'postcircumfix:<[ ]>' :method :multi(_, ['Integer']) 
    .param int n
    .local pmc values, elem
    if n < 0 goto fail
    $I0 = n + 1
    values = self.'!generate'($I0)
    elem = values[n]
    if null elem goto fail
    .return (elem)
  fail:
    .local pmc key
    key = box n
    elem = new ['Proxy']
    setattribute elem, '$!base', values
    setattribute elem, '$!key', key
    $P0 = get_hll_global ['Bool'], 'True'
    setprop elem, 'rw', $P0
    .return (elem)
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
    $P0.'!STORE'(parcel)
    $P1 = new ['Perl6Scalar'], $P0
    .return ($P1)
.end

=head2 Private methods

=over 4

=item !STORE(source)

=cut

.namespace ['Array']
.sub '!STORE' :method
    .param pmc source
    $P0 = get_hll_global 'Seq'
    $P0 = find_method $P0, '!STORE'
    self.$P0(source)
    .local pmc items, items_it, true
    items = getattribute self, '@!items'
    items_it = iter items
    true = get_hll_global ['Bool'], 'True'
  items_loop:
    unless items_it goto items_done
    $P0 = shift items_it
    setprop $P0, 'rw', true
    goto items_loop
  items_done:
    .return (self)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
