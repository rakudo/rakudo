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
    .param pmc parcel          :slurpy
    .local pmc newlist
    newlist = self.'CREATE'()
    setattribute newlist, '@!rest', parcel
    .return (newlist)
.end


.namespace ['List']
.sub 'eager' :method
    .local pmc items
    items = self.'!fill'()
    $P0 = new ['Parcel']
    splice $P0, items, 0, 0
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
    .local pmc items, rest, flat
    flat  = getattribute self, '$!flat'
    items = getattribute self, '@!items'
    rest  = getattribute self, '@!rest'

    if null rest goto rest_null
    rest = clone rest
    goto rest_done
  rest_null:
    rest = clone items
    null items
  rest_done:

    if null items goto items_done
    if flat goto items_flat
    splice rest, items, 0, 0
    null items
    goto items_done
  items_flat:
    items = clone items
  items_done:

    .local pmc flatlist
    flatlist = new ['List']
    flat = get_hll_global 'True'
    setattribute flatlist, '$!flat', flat
    setattribute flatlist, '@!items', items
    setattribute flatlist, '@!rest', rest
    .return (flatlist)
.end


.namespace ['List']
.sub 'item' :method
    $P0 = new ['ObjectRef'], self
    $P1 = get_hll_global 'True'
    setprop $P0, 'scalar', $P1
    .return ($P0)
.end


.sub 'iterator' :method
    .local pmc parceliter, rpa
    rpa = root_new ['parrot';'ResizablePMCArray']
    $P0 = getattribute self, '@!rest'
    if null $P0 goto have_rest
    splice rpa, $P0, 0, 0
  have_rest:
    $P0 = getattribute self, '@!items'
    if null $P0 goto have_items
    splice rpa, $P0, 0, 0
  have_items:
    parceliter = new ['ParcelIter']
    setattribute parceliter, '$!parcel', rpa
    .return (parceliter)
.end


.namespace ['List']
.sub 'list' :method
    .local pmc list
    list = new ['List']
    $P0 = getattribute self, '$!flat'
    setattribute list, '$!flat', $P0
    $P0 = getattribute self, '@!items'
    setattribute list, '@!items', $P0
    $P0 = getattribute self, '@!rest'
    setattribute list, '@!rest', $P0
    .return (list)
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
.sub 'perl' :method
    $P0 = self.'eager'()
    $P0 = $P0.'perl'()
    .return ($P0)
.end


# .namespace ['List']
# .sub 'postcircumfix:<[ ]>' :method :multi(_, _)
#     .param int n
#     .local pmc items, elem
#     items = getattribute self, '@!items'
#     if null items goto fill_items
#     $I0 = elements items
#     if n < $I0 goto have_items
#   fill_items:
#     $I0 = n + 1
#     items = self.'!fill'($I0)
#   have_items:
#     elem = items[n]
#     .return (elem)
# .end

 
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
    if null rest goto done
    .local int items_n
    items_n = elements items
 
  items_loop:
    unless has_n goto rest_loop
    if items_n >= n goto items_done
  rest_loop:
    unless rest goto rest_done
    .local pmc value
    value = shift rest
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
    push items, value
    inc items_n
    goto items_loop
  items_done:
    if rest goto done
  rest_done:
    null rest
    setattribute self, '@!rest', rest
  done:
    .return (items)
.end


.namespace []
.sub '&flat'
    .param pmc values          :slurpy
    .local pmc list, true
    list = new ['List']
    true = get_hll_global 'True'
    setattribute list, '$!flat', true
    setattribute list, '@!rest', values
    .return (list)
.end

