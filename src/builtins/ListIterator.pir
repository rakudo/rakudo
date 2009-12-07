=head1 TITLE

ListIterator - Perl 6 ListIterator class

=head1 DESCRIPTION

Objects in the ListIterator class maintain a reference to a List
and an index to the next item to be iterated.  Shifting the
ListIterator returns the next item from the List and increments
the index.

=head2 Methods

=over 4

=cut

.namespace ['ListIterator']
.sub 'onload' :anon :init :load
    .local pmc p6meta, listproto
    p6meta = get_hll_global ['Mu'], '$!P6META'
    listproto = p6meta.'new_class'('ListIterator', 'parent'=>'Any', 'attr'=>'$!list $!index')
.end

=item shift()

Returns the next item from the List.  If there are no more items,
returns a Failure.

=cut

.sub 'shift' :method
    .local pmc list, index
    unless self goto fail
    list = getattribute self, '$!list'
    index = getattribute self, '$!index'
    $P0 = '!postcircumfix:<[ ]>'(list, index)
    inc index
    .return ($P0)
  fail:
    .tailcall '!FAIL'('Attempt to iterate beyond end of List')
.end


=item Bool()

Returns true if the iterator has not reached the end of the list.

=cut

.sub 'Bool' :method
    .local pmc list, index, values
    list = getattribute self, '$!list'
    index = getattribute self, '$!index'
    values = list.'!generate'(0)
    $I0 = islt index, values
    .return ($I0)
.end

=head2 Private methods

=over 4

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
