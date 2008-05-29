## $Id$

=head1 NAME

src/classes/List.pir - Perl 6 List class and related functions

=head2 Object Methods

=over 4

=cut

.sub 'onload' :anon :load :init
    .local pmc p6meta, listproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    listproto = p6meta.'new_class'('List', 'parent'=>'ResizablePMCArray Any')
    p6meta.'register'('ResizablePMCArray', 'parent'=>listproto, 'protoobject'=>listproto)

    $P0 = split ' ', 'keys kv pairs values'
    .local pmc iter
    iter = new 'Iterator', $P0
  global_loop:
    unless iter goto global_end
    $S0 = shift iter
    $P0 = get_hll_global ['List'], $S0
    set_hll_global $S0, $P0
    goto global_loop
  global_end:

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


=back

=head2 List methods

=over 4

=item !flatten()

Flatten the invocant.

=cut

.sub '!flatten' :method
    .param int size            :optional
    .param int has_size        :opt_flag

    ##  we use the 'elements' opcode here because we want the true length
    .local int len, i
    len = elements self
    i = 0
  flat_loop:
    if i >= len goto flat_end
    unless has_size goto flat_loop_1
    if i >= size goto flat_end
  flat_loop_1:
    .local pmc elem
    elem = self[i]
    $I0 = defined elem
    unless $I0 goto flat_next
    $I0 = isa elem, 'Arrayref'
    if $I0 goto flat_next
    $I0 = does elem, 'array'
    unless $I0 goto flat_next
    splice self, elem, i, 1
    $I0 = elements self
    if i < $I0 goto flat_loop
    goto flat_end
  flat_next:
    inc i
    goto flat_loop
  flat_end:
    $I0 = isa self, 'List'
    if $I0 goto end
    $P0 = new 'List'
    splice $P0, self, 0, 0
    copy self, $P0
  end:
    .return (self)
.end


=item elems()

Return the number of elements in the list.

=cut

.sub 'elems' :method
    $I0 = elements self
    .return ($I0)
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



=item iterator()

Returns an iterator for the list.

=cut

.sub 'iterator' :method
    $P0 = iter self
    .return ($P0)
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


=item join(SEPARATOR)

Returns a string comprised of all of the list, separated by the string SEPARATOR.  Given an empty list, join returns the empty string.

=cut

.sub 'join' :method
    .param string sep
    $S0 = join sep, self
    .return ($S0)
.end


=item keys()

Returns a List containing the keys of the invocant.

=cut

.sub 'keys' :method :multi(ResizablePMCArray)
    $I0 = self.'elems'()
    dec $I0
    .return 'infix:..'(0, $I0)
.end


=item kv()

Return items in invocant as 2-element (index, value) lists.

=cut

.sub 'kv' :method :multi(ResizablePMCArray)
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
    (mapres :slurpy) = expression(args :flat)
    unless mapres goto map_loop
    mapres.'!flatten'()
    $I0 = elements res
    splice res, mapres, $I0, 0
    goto map_loop

  done:
    .return(res)
.end


=item pairs()

Return a list of Pair(index, value) elements for the invocant.

=cut

.sub 'pairs' :method :multi(ResizablePMCArray)
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


=item reverse()

Returns a list of the elements in reverse order.

=cut

.sub 'reverse' :method :multi(ResizablePMCArray)
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


=item sort()

Sort list by copying into FPA, sorting and creating new List.

=cut

.sub 'sort' :method :multi(ResizablePMCArray)
    .param pmc by              :optional
    .param int has_by          :opt_flag
    .local pmc elem, arr
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
    if has_by goto do_sort
    get_hll_global by, 'infix:cmp'
  do_sort:
    # Sort in-place
    arr.'sort'(by)

    # and return new List.
    $P0 = get_hll_global 'list'
    .return $P0(arr)
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


=item values()

Returns a List containing the values of the invocant.

=cut

.sub 'values' :method :multi(ResizablePMCArray)
    .return (self)
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
    .return args.'!flatten'()
.end


=item C<sort>

Sort arguments using (optional) comparison sub.

=cut

.sub 'sort'
    .param pmc args            :slurpy
    .local pmc by
    by = get_hll_global 'infix:cmp'
    unless args goto have_by
    $P0 = args[0]
    $I0 = isa $P0, 'Sub'
    unless $I0 goto have_by
    by = shift args
  have_by:
    args.'!flatten'()
    .return args.'sort'(by)
.end


=item C<map>

Functional form of C<map>. Delegates map to passed list.

=cut

.sub 'map'
    .param pmc expression
    .param pmc values          :slurpy

    values.'!flatten'()
    .return values.'map'(expression)
.end


=item C<infix:,(...)>

Operator form for building a list from its arguments.

=cut

.sub 'infix:,'
    .param pmc args            :slurpy
    .return args.'!flatten'()
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

## TODO: zip

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
