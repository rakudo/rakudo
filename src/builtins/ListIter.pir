=head1 TITLE

ListIter - Perl 6 List iterator

=head1 DESCRIPTION

ListIter is used to iterate over a List.
We implement it in PIR for speed, as list iteration
is a fairly common operation.

=head2 Methods

=over 4

=cut

.namespace ['ListIter']
.sub 'onload' :anon :init :load
    .local pmc p6meta, proto
    p6meta = get_hll_global ['Mu'], '$!P6META'
    proto = p6meta.'new_class'('ListIter', 'parent'=>'Iterator', 'attr'=>' @!list @!reify')
.end

.namespace ['ListIter']
.sub 'new' :method
    .param pmc list
    .local pmc listiter
    listiter = new ['ListIter']
    setattribute listiter, '@!list', list
    .return (listiter)
.end
    

.namespace ['ListIter']
.sub 'reify' :method
    .local pmc reify, list, rest, nextiter
    # if this iterator already reified, return its last result
    reify = getattribute self, '@!reify'
    unless null reify goto iter_reified
    # fill our target list with at least one element, and then
    # steal its entire list of reified @!items for us to return
    list = getattribute self, '@!list'
    reify = list.'!fill'(1)
    setattribute self, '@!reify', reify
    null $P0
    setattribute list, '@!items', $P0
    # if the List still has elements, append a new ListIter to handle them
    rest = getattribute list, '@!rest'
    unless rest goto iter_reified
    nextiter = new ['ListIter']
    setattribute nextiter, '@!list', list
    push reify, nextiter
  iter_reified:
    .return (reify)
.end
    
=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
#vim: expandtab shiftwidth=4 ft=pir:
