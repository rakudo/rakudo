=head1 TITLE

List - Perl 6 List class

This file implements Perl 6 lists.
(It's temporarily named "List" to avoid conflict with existing
List classes while we convert to the new list model.)

=head1 DESCRIPTION

=head2 Methods

=over 4

=cut

.namespace ['List']
.sub 'onload' :anon :init :load
    .local pmc p6meta, listproto
    p6meta = get_hll_global ['Mu'], '$!P6META'
    
    # Create the class.
    listproto = p6meta.'new_class'('List', 'parent'=>'Iterable', 'attr'=>'$!flat @!items @!rest')
.end


.sub 'new' :method
    .param pmc values          :slurpy
    .local pmc p6meta, parrotclass, list, true
    p6meta = get_hll_global ['Mu'], '$!P6META'
    parrotclass = p6meta.'get_parrotclass'(self)
    list = new parrotclass
    setattribute list, '@!rest', values
    transform_to_p6opaque list
    .return (list)
.end


.namespace ['List']
.sub 'Capture' :method
    $P0 = self.'!fill'()
    $P1 = get_hll_global 'Capture'
    $P1 = $P1.'new'($P0 :flat)
    .return ($P1)
.end


.namespace ['List']
.sub 'eager' :method
    .local pmc items
    self.'!fill'()
    $P0 = descalarref self
    .return ($P0)
.end


.namespace ['List']
.sub 'elems' :method
    .local pmc items
    items = self.'!fill'()
    $I0 = elements items
    .return ($I0)
.end


.namespace ['List']
.sub 'flat' :method
    .local pmc list, flat, items, rest
    list = descalarref self
    # If we're already flat, return self
    flat  = getattribute self, '$!flat'
    if null flat goto make_flatlist
    if flat goto done
  make_flatlist:
    items = getattribute list, '@!items'
    rest  = getattribute list, '@!rest'

    if null rest goto rest_done
    rest = clone rest
  rest_done:

    if null items goto items_done
    if null rest goto items_rest
    splice rest, items, 0, 0
    goto items_done
  items_rest:
    rest = clone items
  items_done:
    null items

    list = new ['List']
    flat = get_hll_global 'True'
    setattribute list, '$!flat', flat
    setattribute list, '@!items', items
    setattribute list, '@!rest', rest
  done:
    .return (list)
.end


.namespace ['List']
.sub 'iterator' :method
    $P0 = get_hll_global 'ListIter'
    $P1 = self.'!List'()
    $P0 = $P0.'new'($P1)
    .return ($P0)
.end


.namespace ['List']
.sub 'list' :method
    $P0 = descalarref self
    .return ($P0)
.end


.namespace ['List']
.sub 'munch' :method
    .param int n
    .local pmc items, parcel
    items = self.'!fill'(n)
    parcel = new ['Parcel']
  loop:
    unless items goto done
    unless n > 0 goto done
    $P0 = shift items
    push parcel, $P0
    dec n
    goto loop
  done:
    .return (parcel)
.end


.namespace ['List']
.sub '!List' :method
    .local pmc list, flat, items, rest
    list  = new ['List']
    flat  = getattribute self, '$!flat'
    items = getattribute self, '@!items'
    rest  = getattribute self, '@!rest'

    setattribute list, '$!flat', flat
    if null items goto items_done
    items = clone items
    setattribute list, '@!items', items
  items_done:
    if null rest goto rest_done
    rest = clone rest
    setattribute list, '@!rest', rest
  rest_done:
    .return (list)
.end

.namespace ['List']
.sub '!elem' :method
    .param pmc value
    unless null value goto done
    value = new ['Perl6Scalar']
  done:
    .return (value)
.end

 
.namespace ['List']
.sub '!fill' :method
    .param int n               :optional
    .param int has_n           :opt_flag

    .local pmc flat, items, rest
    flat  = getattribute self, '$!flat'
    items = getattribute self, '@!items'
    rest  = getattribute self, '@!rest'

    unless null items goto have_items
    items = root_new ['parrot';'ResizablePMCArray']
    setattribute self, '@!items', items
  have_items:
    unless null rest goto have_rest
    rest = root_new ['parrot';'ResizablePMCArray']
    setattribute self, '@!rest', rest
  have_rest:

    unless rest goto done
    .local int items_n
    items_n = elements items
 
  items_loop:
    unless has_n goto rest_loop
    if items_n >= n goto items_done
  rest_loop:
    unless rest goto rest_done
    .local pmc value
    value = shift rest
    if null value goto value_item
    $I0 = isa value, ['EMPTY']
    if $I0 goto rest_loop
    $I0 = isa value, ['Iterator']
    if $I0 goto value_iterator
    if null flat goto value_item
    unless flat goto value_item
    $P0 = getprop 'scalar', value
    unless null $P0 goto value_item
    $I0 = isa value, ['ResizablePMCArray']
    if $I0 goto value_rpa
    $I0 = isa value, ['Iterable']
    unless $I0 goto value_item
    value = value.'iterator'()
  value_iterator:
    value = value.'reify'()
  value_rpa:
    splice rest, value, 0, 0
    goto rest_loop
  value_item:
    value = self.'!elem'(value)
    push items, value
    inc items_n
    goto items_loop
  items_done:
  rest_done:
  done:
    .return (items)
.end


.namespace ['List']
.sub '!splice' :method
    .param pmc repl
    .param int offset
    .param int size            :optional
    .param int has_size        :opt_flag


    .local pmc nil, items, rest
    nil = root_new ['parrot';'ResizablePMCArray']

    .local int fill
    fill = offset
    unless has_size goto have_fill
    if size <= 0 goto have_fill
    fill += size
  have_fill:

    .local pmc items, rest, retlist, retitems, retrest
    items = self.'!fill'(offset)
    rest = getattribute self, '@!rest'

    # We can figure out how to proceed based on how many
    # items we were able to reify
    .local int items_n
    items_n = elements items

    # If we didn't get at least C<offset> items, then C<rest>
    # is already empty, and we finish up with an empty retlist.
    if items_n < offset goto empty_retlist
    # If the retlist eats all remaining elements, do that
    unless has_size goto splice_retlist
    # If the retlist is explicitly empty, do that
    if size <= 0 goto empty_retlist
    # If there are reified elements for the retlist, build it
    if items_n > offset goto splice_retlist
  empty_retlist:
    $P0 = get_hll_global 'List'
    retlist = $P0.'new'()
    goto retlist_done
  splice_retlist:
    retlist = self.'!List'()
    retitems = getattribute retlist, '@!items'
    retrest = getattribute retlist, '@!rest'
    if has_size goto retlist_sized
    # The retlist gets everything after offset.
    rest = 0
    goto retlist_items
  retlist_sized:
    # Everything after fill gets chopped from the retlist
    retrest = 0
    if items_n <= fill goto retlist_items
    assign retitems, fill
  retlist_items:
    # ...and we chop the first offset items from the retlist
    splice retitems, nil, 0, offset
  retlist_done:

    # If there are any items after the fill point,
    # we have to move them back into rest
    if items_n <= fill goto move_done
    .local pmc move
    move = clone items
    splice move, nil, 0, fill
    splice rest, move, 0, 0
  move_done:
    # chomp all but the first offset items from our reified list
    if items_n <= offset goto items_done
    assign items, offset
  items_done:
    # Add the replacement list to our rest
    repl = repl.'iterator'()
    unshift rest, repl
    # return the return list
    .return (retlist)
.end
    

.namespace []
.sub '&flat'
    .param pmc values          :slurpy
    .local pmc list, true
    list = new ['List']
    true = get_hll_global 'True'
    setattribute list, '$!flat', true
    setattribute list, '@!rest', values
    transform_to_p6opaque list
    .return (list)
.end


.namespace []
.sub '&list'
    .param pmc values          :slurpy
    .local pmc list
    list = new ['List']
    setattribute list, '@!rest', values
    transform_to_p6opaque list
    .return (list)
.end
