=head1 TITLE

Seq - Perl 6 Seq class

=head1 DESCRIPTION

Seqs are Lists of (immutable) values.

=head2 Methods

=over 4

=cut

.namespace ['Seq']
.sub 'onload' :anon :init :load
    .local pmc p6meta, proto
    p6meta = get_hll_global ['Mu'], '$!P6META'
    proto = p6meta.'new_class'('Seq', 'parent'=>'List')
.end

=item new

=cut

.namespace ['Seq']
.sub 'new' :method
    .param pmc values          :slurpy
    .local pmc p6meta, parrotclass, list, true
    p6meta = get_hll_global ['Mu'], '$!P6META'
    parrotclass = p6meta.'get_parrotclass'(self)
    list = new parrotclass
    setattribute list, '@!rest', values
    $P0 = get_hll_global 'True'
    setattribute list, '$!flat', $P0
    transform_to_p6opaque list
    .return (list)
.end

=item new_from_RPA

Creates a new Seq from a ResizablePMCArray.

=cut

.namespace ['Seq']
.sub 'new_from_RPA' :method
    .param pmc rpa
    .local pmc seq
    seq = self.'new'()
    setattribute seq, '@!rest', rpa
    $P0 = get_hll_global 'True'
    setattribute seq, '$!flat', $P0
    .return (seq)
.end

=back

=head2 Private methods

=over 4

=item !elem(item)

Create an element for the Seq (has the 'rw' property set).

=cut

.namespace ['Seq']
.sub '!elem' :method
    .param pmc item
    unless null item goto have_item
    item = get_hll_global 'Any'
    .return (item)
  have_item:
    item = descalarref item
    $I0 = can item, 'item'
    unless $I0 goto done
    item = item.'item'()
  done:
    .return (item)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
