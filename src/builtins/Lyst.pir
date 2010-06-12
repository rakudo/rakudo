=head1 TITLE

Lyst - Perl 6 List class

This file implements Perl 6 lists.
(It's temporarily named "Lyst" to avoid conflict with existing
List classes while we convert to the new list model.)

=head1 DESCRIPTION

=head2 Methods

=over 4

=cut

.namespace ['Lyst']
.sub 'onload' :anon :init :load
    .local pmc p6meta, listproto
    p6meta = get_hll_global ['Mu'], '$!P6META'
    
    # Create the class.
    listproto = p6meta.'new_class'('Lyst', 'parent'=>'Iterable', 'attr'=>'$!flat @!items @!rest')
.end


.sub 'new' :method
    .param pmc parcel          :slurpy
    .local pmc newlist
    newlist = self.'CREATE'()
    setattribute newlist, '@!rest', parcel
    .return (newlist)
.end


.namespace ['Lyst']
.sub 'eager' :method
    .local pmc items
    items = self.'!fill'()
    $P0 = new ['Parcel']
    splice $P0, items, 0, 0
    .return ($P0)
.end


.namespace ['Lyst']
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
    flatlist = new ['Lyst']
    flat = get_hll_global 'True'
    setattribute flatlist, '$!flat', flat
    setattribute flatlist, '@!items', items
    setattribute flatlist, '@!rest', rest
    .return (flatlist)
.end


.namespace ['Lyst']
.sub 'perl' :method
    $P0 = self.'eager'()
    $P0 = $P0.'perl'()
    .return ($P0)
.end


.namespace ['Lyst']
.sub 'postcircumfix:<[ ]>' :method :multi(_, ['Integer'])
    .param int n
    .local pmc items, elem
    items = getattribute self, '@!items'
    $I0 = elements items
    if n < $I0 goto have_items
    $I0 = n + 1
    items = self.'!fill'($I0)
  have_items:
    elem = items[n]
    .return (elem)
.end

 
.namespace ['Lyst']
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


