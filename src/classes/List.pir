## $Id$

=head1 NAME

src/classes/List.pir - Perl 6 List class and related functions

=head2 Methods

=over 4

=cut

.sub 'onload' :anon :load :init
    .local pmc p6meta, listproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    listproto = p6meta.'new_class'('List', 'parent'=>'ResizablePMCArray Any')
    p6meta.'register'('ResizablePMCArray', 'parent'=>listproto, 'protoobject'=>listproto)
.end


=item clone()    (vtable method)

Return a clone of this list.  (Clones its elements also.)

=cut

.namespace ['List']
.sub 'clone' :vtable :method
    .local pmc p6meta, result, iter
    $P0 = typeof self
    result = new $P0
    iter = self.'iterator'()
  iter_loop:
    unless iter goto iter_end
    $P0 = shift iter
    $P0 = clone $P0
    push result, $P0
    goto iter_loop
  iter_end:
    .return (result)
.end


=item get_string()    (vtable method)

Return the elements of the list joined by spaces.

=cut


.sub 'get_string' :vtable :method
    $S0 = join ' ', self
    .return ($S0)
.end


=item item()

Return the List invocant in scalar context (i.e., an Arrayref).

=cut

.sub 'item' :method
    .return '!Arrayref'(self)
.end

=item list()

Return the List as a list.

=cut

.sub 'list' :method
    .return (self)
.end


=item iterator()

Returns an iterator for the list.

=cut

.sub 'iterator' :method
    $P0 = iter self
    .return ($P0)
.end


=item perl()

Returns a Perl representation of a List.

=cut

.sub 'perl' :method
    .local string result
    result = '['

    .local pmc iter
    iter = self.'iterator'()
    unless iter goto iter_done
  iter_loop:
    $P1 = shift iter
    $S1 = $P1.'perl'()
    result .= $S1
    unless iter goto iter_done
    result .= ', '
    goto iter_loop
  iter_done:
    result .= ']'
    .return (result)
.end


=item elems()

Return the number of elements in the list.

=cut

.sub 'elems' :method
    $I0 = elements self
    .return ($I0)
.end


=item keys()

Returns a List containing the keys of the List.

=cut

.sub 'keys' :method
    $I0 = self.'elems'()
    dec $I0
    .return 'infix:..'(0, $I0)
.end


=item values()

Returns a List containing the values of the List.

=cut

.sub 'values' :method
    .return (self)
.end


=item unshift(ELEMENTS)

Prepends ELEMENTS to the front of the list.

=cut

.sub 'unshift' :method
    .param pmc args :slurpy

  loop:
    unless args goto done
    .local pmc val
    val = pop args
    unshift self, val
    goto loop
  done:
    .return self.'elems'()
.end


=item shift()

Shifts the first item off the list and returns it.

=cut

.sub 'shift' :method
    .local pmc x
    .local int len

    len = self.'elems'()
    if len < 1 goto empty
    x = shift self
    goto done
  empty:
    x = new 'Failure'
  done:
    .return (x)
.end


=item pop()

Treats the list as a stack, popping the last item off the list and returning it.

=cut

.sub 'pop' :method
    .local pmc x
    .local int len

    len = self.'elems'()
    if len < 1 goto empty
    x = pop self
    goto done
  empty:
    x = new 'Failure'
  done:
    .return (x)
.end


=item push(ELEMENTS)

Treats the list as a stack, pushing ELEMENTS onto the end of the list.  Returns the new length of the list.

=cut

.sub 'push' :method
    .param pmc args :slurpy
  loop:
    unless args goto done
    $P0 = shift args
    push self, $P0
    goto loop
  done:
    .return self.'elems'()
.end


=item join(SEPARATOR)

Returns a string comprised of all of the list, separated by the string SEPARATOR.  Given an empty list, join returns the empty string.

=cut

.sub 'join' :method
    .param string sep
    $S0 = join sep, self
    .return ($S0)
.end


=item reverse()

Returns a list of the elements in revese order.

=cut

.sub 'reverse' :method
    .local pmc result, iter
    result = new 'List'
    iter = self.'iterator'()
  iter_loop:
    unless iter goto iter_done
    $P0 = shift iter
    unshift result, $P0
    goto iter_loop
  iter_done:
    .return (result)
.end

=item delete()

Deletes the given elements from the List, replacing them with null.  Returns a List of removed elements.

=cut

.sub delete :method
    .param pmc indices :slurpy
    .local pmc result
    result = new 'List'
    null $P99

  indices_loop:
    unless indices goto indices_end
    $I0 = shift indices
    $P0 = self[$I0]
    push result, $P0
    self[$I0] = $P99

  shorten:
    $I0 = self.'elems'()
    dec $I0
  shorten_loop:
    if $I0 < 0 goto shorten_end
    $P0 = self[$I0]
    unless null $P0 goto shorten_end
    delete self[$I0]
    dec $I0
    goto shorten_loop
  shorten_end:
    goto indices_loop

  indices_end:
    .return (result)
.end


=item exists(INDEX)

Checks to see if the specified index or indices have been assigned to.  Returns a Bool value.

=cut

.sub exists :method
    .param pmc indices :slurpy
    .local int test

    test = 0
  indices_loop:
    unless indices goto indices_end
    $I0 = shift indices
    test = exists self[$I0]
    if test goto indices_loop
  indices_end:
    .return 'prefix:?'(test)
.end


=item kv()

=cut

.sub kv :method
    .local pmc result, iter
    .local int i

    result = new 'List'
    iter = self.'iterator'()
    i = 0
  iter_loop:
    unless iter goto iter_end
    $P0 = shift iter
    push result, i
    push result, $P0
    inc i
    goto iter_loop
  iter_end:
    .return (result)
.end

=item pairs()

=cut

.sub pairs :method
    .local pmc result, iter
    .local int i

    result = new 'List'
    iter = self.'iterator'()
    i = 0
  iter_loop:
    unless iter goto iter_end
    $P0 = shift iter
    $P1 = 'infix:=>'(i, $P0)
    push result, $P1
    inc i
    goto iter_loop
  iter_end:
    .return (result)
.end


=item grep(...)

=cut

.sub grep :method
    .param pmc test
    .local pmc retv
    .local pmc iter
    .local pmc block_res
    .local pmc block_arg

    retv = new 'List'
    iter = new 'Iterator', self
  loop:
    unless iter goto done
    block_arg = shift iter
    block_res = test(block_arg)

    unless block_res goto loop
    retv.'push'(block_arg)
    goto loop

  done:
    .return(retv)
.end

=item reduce(...)

=cut

.sub reduce :method
    .param pmc oper
    .local pmc retv
    .local pmc iter
    .local pmc block_res
    .local pmc block_arg

    retv = new 'List'
    iter = new 'Iterator', self
    unless iter goto empty

    retv = shift iter

  loop:
    unless iter goto done
    block_arg = shift iter
    block_res = oper(retv, block_arg)
    goto loop

  empty:
    retv = new 'Undef'
    goto done

  done:
    .return(retv)
.end

=item first(...)

=cut

.sub first :method
    .param pmc test
    .local pmc retv
    .local pmc iter
    .local pmc block_res
    .local pmc block_arg

    iter = new 'Iterator', self

  loop:
    unless iter goto nomatch
    block_arg = shift iter
    block_res = test(block_arg)
    if block_res goto matched
    goto loop

  matched:
    retv = block_arg
    goto done

  nomatch:
    retv = new 'Undef'
    goto done

  done:
    .return(retv)
.end

=item uniq(...)

=cut

# TODO Rewrite it. It's too naive.

.sub uniq :method
    .local pmc ulist
    .local pmc key
    .local pmc val
    .local pmc uval
    .local int len
    .local int i
    .local int ulen
    .local int ui

    ulist = new 'List'
    len = self.'elems'()
    i = 0

  loop:
    if i == len goto done

    val = self[i]

    ui = 0
    ulen = ulist.'elems'()
    inner_loop:
        if ui == ulen goto inner_loop_done

        uval = ulist[ui]
        if uval == val goto found

        inc ui
        goto inner_loop
    inner_loop_done:

    ulist.'push'(val)

    found:

    inc i
    goto loop

  done:
    .return(ulist)
.end

=item sort()

Sort list by copying into FPA, sorting and creating new List.

=cut

.sub 'sort' :method
    .param pmc comparer :optional
    .param int have_comparer :opt_flag
    .local pmc elem, arr, comparer
    .local int len, i

    # Creating FPA
    arr = new 'FixedPMCArray'
    len = self.'elems'()
    arr = len

    # Copy all elements into it
    i = 0
  copy_to:
    if i == len goto done_to
    elem = self[i]
    arr[i] = elem
    inc i
    goto copy_to

  done_to:

    # Check comparer
    if have_comparer goto do_sort
    get_hll_global comparer, 'infix:cmp'

  do_sort:

    # Sort in-place
    arr.'sort'(comparer)

    # and return new List.
    $P0 = get_hll_global 'list'
    .return $P0(arr)
.end


=item map()

Map.

=cut

.sub 'map' :method
    .param pmc expression
    .local pmc res, elem, block, mapres, iter, args
    .local int i, arity

    arity = expression.'arity'()
    if arity > 0 goto body
    arity = 1
  body:
    res = new 'List'
    iter = self.'iterator'()
  map_loop:
    unless iter goto done

    # Creates arguments for closure
    args = new 'ResizablePMCArray'

    i = 0
  args_loop:
    if i == arity goto invoke
    unless iter goto elem_undef
    elem = shift iter
    goto push_elem
  elem_undef:
    elem = new 'Failure'
  push_elem:
    push args, elem
    inc i
    goto args_loop

  invoke:
    mapres = expression(args :flat)
    if null mapres goto map_loop

  push_res:
    res.'push'(mapres)
    goto map_loop

  done:
    .return(res)
.end

=back

=head1 Functions

=over 4

=item C<list(...)>

Build a List from its arguments.

=cut

.namespace

.sub 'list'
    .param pmc args            :slurpy
    .local pmc list, item
    list = new 'List'
  args_loop:
    unless args goto args_end
    item = shift args
    $I0 = defined item
    unless $I0 goto add_item
    $I0 = isa item, 'Arrayref'
    if $I0 goto add_item
    $I0 = does item, 'array'
    unless $I0 goto add_item
    splice args, item, 0, 0
    goto args_loop
  add_item:
    push list, item
    goto args_loop
  args_end:
    .return (list)
.end


=item C<sort>

Sort arguments using (optional) comparition sub.

=cut

.sub 'sort'
    .param pmc comparer :optional
    .param pmc args :slurpy
    .local pmc l

    $I0 = isa comparer, 'Sub'
    if $I0 goto with_cmp
    l = 'list'(comparer, args :flat)
    .return l.'sort'()

  with_cmp:
    l = 'list'(args :flat)
    .return l.'sort'(comparer)
.end


=item C<map>

Operator form of C<map>. Delegates map to passed list.

=cut

.sub 'map' :multi(_,List)
    .param pmc expression
    .param pmc list

    .return list.'map'(expression)
.end


=item C<infix:,(...)>

Operator form for building a list from its arguments.

=cut

.sub 'infix:,'
    .param pmc args            :slurpy
    .return 'list'(args :flat)
.end


=item C<infix:Z(...)>

The zip operator.

=cut

.sub 'infix:Z'
    .param pmc args :slurpy
    .local int num_args
    num_args = elements args

    # Empty list of no arguments.
    if num_args > 0 goto has_args
    $P0 = new 'List'
    .return($P0)
has_args:

    # Get minimum element count - what we'll zip to.
    .local int min_elem
    .local int i
    i = 0
    $P0 = args[0]
    min_elem = elements $P0
min_elems_loop:
    if i >= num_args goto min_elems_loop_end
    $P0 = args[i]
    $I0 = elements $P0
    unless $I0 < min_elem goto not_min
    min_elem = $I0
not_min:
    inc i
    goto min_elems_loop
min_elems_loop_end:

    # Now build result list of lists.
    .local pmc res
    res = new 'List'
    i = 0
zip_loop:
    if i >= min_elem goto zip_loop_end
    .local pmc cur_list
    cur_list = new 'List'
    .local int j
    j = 0
zip_elem_loop:
    if j >= num_args goto zip_elem_loop_end
    $P0 = args[j]
    $P0 = $P0[i]
    cur_list[j] = $P0
    inc j
    goto zip_elem_loop
zip_elem_loop_end:
    res[i] = cur_list
    inc i
    goto zip_loop
zip_loop_end:

    .return(res)
.end


=item C<infix:X(...)>

The non-hyper cross operator.

=cut

.sub 'infix:X'
    .param pmc args            :slurpy
    .local pmc res
    res = new 'List'

    # Algorithm: we'll maintain a list of counters for each list, incrementing
    # the counter for the right-most list and, when it we reach its final
    # element, roll over the counter to the next list to the left as we go.
    .local pmc counters
    .local pmc list_elements
    .local int num_args
    counters = new 'FixedIntegerArray'
    list_elements = new 'FixedIntegerArray'
    num_args = elements args
    counters = num_args
    list_elements = num_args

    # Get element count for each list.
    .local int i
    .local pmc cur_list
    i = 0
elem_get_loop:
    if i >= num_args goto elem_get_loop_end
    cur_list = args[i]
    $I0 = elements cur_list
    list_elements[i] = $I0
    inc i
    goto elem_get_loop
elem_get_loop_end:

    # Now we'll start to produce them.
    .local int res_count
    res_count = 0
produce_next:

    # Start out by building list at current counters.
    .local pmc new_list
    new_list = new 'List'
    i = 0
cur_perm_loop:
    if i >= num_args goto cur_perm_loop_end
    $I0 = counters[i]
    $P0 = args[i]
    $P1 = $P0[$I0]
    new_list[i] = $P1
    inc i
    goto cur_perm_loop
cur_perm_loop_end:
    res[res_count] = new_list
    inc res_count

    # Now increment counters.
    i = num_args - 1
inc_counter_loop:
    $I0 = counters[i]
    $I1 = list_elements[i]
    inc $I0
    counters[i] = $I0

    # In simple case, we just increment this and we're done.
    if $I0 < $I1 goto inc_counter_loop_end

    # Otherwise we have to carry.
    counters[i] = 0

    # If we're on the first element, all done.
    if i == 0 goto all_done

    # Otherwise, loop.
    dec i
    goto inc_counter_loop
inc_counter_loop_end:
    goto produce_next

all_done:
    .return(res)
.end


=item C<infix:min(...)>

The min operator.

=cut

.sub 'infix:min'
    .param pmc args :slurpy

    # If we have no arguments, undefined.
    .local int elems
    elems = elements args
    if elems > 0 goto have_args
    $P0 = new 'Undef'
    .return($P0)
have_args:

    # Find minimum.
    .local pmc cur_min
    .local int i
    cur_min = args[0]
    i = 1
find_min_loop:
    if i >= elems goto find_min_loop_end
    $P0 = args[i]
    $I0 = 'infix:cmp'($P0, cur_min)
    if $I0 != -1 goto not_min
    set cur_min, $P0
not_min:
    inc i
    goto find_min_loop
find_min_loop_end:

    .return(cur_min)
.end


=item C<infix:max(...)>

The max operator.

=cut

.sub 'infix:max'
    .param pmc args :slurpy

    # If we have no arguments, undefined.
    .local int elems
    elems = elements args
    if elems > 0 goto have_args
    $P0 = new 'Undef'
    .return($P0)
have_args:

    # Find maximum.
    .local pmc cur_max
    .local int i
    cur_max = args[0]
    i = 1
find_max_loop:
    if i >= elems goto find_max_loop_end
    $P0 = args[i]
    $I0 = 'infix:cmp'($P0, cur_max)
    if $I0 != 1 goto not_max
    set cur_max, $P0
not_max:
    inc i
    goto find_max_loop
find_max_loop_end:

    .return(cur_max)
.end

=item C<reverse(LIST)>

Returns the elements of LIST in the opposite order.

=cut

.sub 'reverse'
    .param pmc list :slurpy
    .local string type
    .local pmc retv
    .local pmc elem
    .local int len
    .local int i

    len = elements list

    if len > 1 goto islist

    # If we're not a list, check if we're a string.
    elem = list[0]
    typeof type, elem

    # This is a bit of a work around - some operators (ie. ~) return
    # a String object instead of a Perl6String.
    eq type, 'String', parrotstring
    eq type, 'Perl6Str', perl6string
    goto islist

  parrotstring:
    .local string tmps
    tmps = elem
    elem = new 'Perl6Str'
    elem = tmps

  perl6string:
    retv = elem.'reverse'()
    goto done

  islist:
    retv = new 'List'
    i = 0

  loop:
    if i == len goto done
    elem = list[i]
    retv.'unshift'(elem)
    inc i
    goto loop

  done:
    .return(retv)
.end

.sub keys :multi('List')
  .param pmc list

  .return list.'keys'()
.end

.sub values :multi('List')
  .param pmc list

  .return list.'values'()
.end

.sub delete :multi('List')
  .param pmc list
  .param pmc indices :slurpy

  .return list.'delete'(indices :flat)
.end

.sub exists :multi('List')
  .param pmc list
  .param pmc indices :slurpy

  .return list.'exists'(indices :flat)
.end

.sub kv :multi('List')
    .param pmc list

    .return list.'kv'()
.end

.sub pairs :multi('List')
    .param pmc list

    .return list.'pairs'()
.end

.sub grep :multi(_,'List')
    .param pmc test
    .param pmc list :slurpy

    .return list.'grep'(test)
.end

.sub reduce :multi(_,'List')
    .param pmc test
    .param pmc list

    .return list.'reduce'(test)
.end


.sub first :multi(_,'List')
    .param pmc test
    .param pmc list :slurpy

    .return list.'first'(test)
.end

.sub uniq :multi('List')
    .param pmc list

    .return list.'uniq'()
.end

.sub 'pop' :multi('List')
    .param pmc list

    .return list.'pop'()
.end


## TODO: zip

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
