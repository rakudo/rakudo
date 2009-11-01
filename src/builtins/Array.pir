=head1 TITLE

Array - Perl 6 Array class

=head1 DESCRIPTION

Arrays are the mutable form of Lists.

=head2 Methods

=over 4

=cut

.namespace ['Array']
.sub 'onload' :anon :init :load
    .local pmc p6meta, listproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    listproto = p6meta.'new_class'('Array', 'parent'=>'List')
.end

=item postcircumfix:<[ ]>(Int)

=cut

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
    $P0 = new ['Array']
    setattribute $P0, '$!values', values
    .return ($P0)
.end

=head2 Private methods

=over 4

=item !STORE(source)

=cut

.namespace ['Array']
.sub '!STORE' :method
    .param pmc source
    .local pmc values, source, source_it, true
    values = root_new ['parrot';'ResizablePMCArray']
    true = get_hll_global ['Bool'], 'True'
    source = '&list'(source)
    source_it = iter source
  array_loop:
    unless source_it goto array_done
    $P0 = shift source_it
    $P1 = new ['ObjectRef']
    setprop $P1, 'rw', true
    push values, $P1
    $P1.'!STORE'($P0)
    goto array_loop
  array_done:
    $I0 = elements values
    $P0 = box $I0
    setattribute self, '$!values', values
    setattribute self, '$!gen', $P0
    .return (self)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
