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
    .return (list)
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
.sub 'item' :method
    $P0 = descalarref self
    $P0 = new ['ObjectRef'], $P0
    $P1 = get_hll_global 'True'
    setprop $P0, 'scalar', $P1
    .return ($P0)
.end


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
.sub 'perl' :method
    self.'eager'()
    .local pmc results
    results = root_new ['parrot';'ResizableStringArray']
    .local pmc it
    $P0 = getattribute self, '@!items'
    if null $P0 goto items_done
    it = iter $P0
  items_loop:
    unless it goto items_done
    $P0 = shift it
    $P0 = $P0.'perl'()
    push results, $P0
    goto items_loop
  items_done:
    goto rest_done
    $P0 = getattribute self, '@!rest'
    if null $P0 goto rest_done
    it = iter $P0
  rest_loop:
    unless it goto rest_done
    $P0 = shift it
    $P0 = $P0.'perl'()
    push results, $P0
    goto rest_loop
  rest_done:
    .local string joined
    joined = join ', ', results
    $P0 = getprop 'scalar', self
    if null $P0 goto self_list
    unless $P0 goto self_list
  self_item:
    joined = concat '[', joined
    joined = concat joined, ']'
    goto done
  self_list:
    joined = concat '(', joined
    joined = concat joined, ')'
  done:
    .return (joined)
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

