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
    .local pmc p6meta, parrotclass, seq, true
    p6meta = get_hll_global ['Mu'], '$!P6META'
    parrotclass = p6meta.'get_parrotclass'(self)
    seq = new parrotclass
    true = get_hll_global 'True'
    setattribute seq, '$!flat', true
    setattribute seq, '@!rest', values
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
    item = new ['Perl6Scalar']
  have_item:
    item = descalarref item
    .return (item)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
