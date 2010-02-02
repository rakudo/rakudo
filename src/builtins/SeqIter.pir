=head1 TITLE

SeqIter - Perl 6 sequence iterator class

=head1 DESCRIPTION

SeqIter is used to iterate over the elements of a Seq
(and Array).  Each Seqiterator maintains a reference to the
Seq object that it is iterating, and the index of the next
item to be retrieved.

=head2 Methods

=over 4

=cut

.namespace ['SeqIter']
.sub 'onload' :anon :init :load
    .local pmc p6meta, proto
    p6meta = get_hll_global ['Mu'], '$!P6META'
    proto = p6meta.'new_class'('SeqIter', 'parent'=>'Iterator', 'attr'=>'$!seq $!index')
.end


=item get()

Return the next item from the Seq.  Returns EMPTY when
all items have been returned.

=cut

.sub 'get' :method
    .local pmc seq, index, items
    seq   = getattribute self, '$!seq'
    index = getattribute self, '$!index'
    items = getattribute seq,  '@!items'

    .local int n
    n = index
    $I0 = elements items
    if n < $I0 goto have_items
    $I0 = n + 1
    items = seq.'!fill'($I0)
    $I0 = elements items
    if n < $I0 goto have_items
    $P0 = get_hll_global 'EMPTY'
    .return ($P0)
  have_items:
    inc index
    $P0 = items[n]
    .return ($P0)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
