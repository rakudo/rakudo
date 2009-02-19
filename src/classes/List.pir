## $Id$

=head1 NAME

src/classes/List.pir - Perl 6 List class and related functions

=cut

.namespace []
.sub '' :anon :load :init
    .local pmc p6meta, listproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    listproto = p6meta.'new_class'('List', 'parent'=>'ResizablePMCArray Any')
    $P0 = get_hll_global 'Positional'
    $P0 = $P0.'!select'()
    p6meta.'add_role'($P0, 'to'=>listproto)
    p6meta.'register'('ResizablePMCArray', 'parent'=>listproto, 'protoobject'=>listproto)

.end

=head2 Methods

=over

=item ACCEPTS

Smart-matches against the list.

=cut

.namespace ['List']
.sub 'ACCEPTS' :method
    .param pmc topic

    # What do we have?
    $I0 = isa topic, 'List' # Catches Array too
    if $I0 goto array
    # XXX When we have a Set type, need to handle that here too.
    topic = topic.'list'()

    # Need to DWIM on *s.
  array:
    .local pmc whatever
    whatever = get_hll_global 'Whatever'
    .local pmc it_a, it_b, cur_a, cur_b
    it_a = iter self
    it_b = iter topic
    unless it_a goto it_loop_end
    unless it_b goto it_loop_end
    cur_a = shift it_a
  it_loop:
    unless it_b goto it_loop_end
    cur_b = shift it_b

    # If there curent thing is Whatever, need special handling.
    $I0 = isa cur_a, whatever
    unless $I0 goto not_whatever

    # If we don't have anything left other than the Whatever, it matches any
    # ending. Otherwise, we see what we're next looking for, and keep pulling
    # from the topic until we see it, or until we run out of topic in which
    # case we can't get no satisfaction.
  handle_whatever:
    unless it_a goto true
    .local pmc looking_for
    looking_for = shift it_a
    $I0 = isa looking_for, whatever
    if $I0 goto handle_whatever
  whatever_loop:
    $P0 = 'infix:==='(looking_for, cur_b)
    if $P0 goto found_looking_for
    unless it_b goto false
    cur_b = shift it_b
    goto whatever_loop
  found_looking_for:
    unless it_a goto it_loop_end
    cur_a = shift it_a
    goto it_loop

  not_whatever:
    # Not whatever - check a against b, and pull another a for the next time
    # around the loop, unless we've run out of b (note that if it's a whatever
    # then it doesn't matter if we ran out of b; if it's not and we ran out of
    # list b then we fail).
    $I0 = 'infix:==='(cur_a, cur_b)
    unless $I0 goto false
    unless it_a goto it_loop_end
    cur_a = shift it_a
    $I0 = isa cur_a, whatever
    if $I0 goto handle_whatever
    unless it_b goto false
    goto it_loop
  it_loop_end:
    if it_a goto false
    if it_b goto false
  true:
    $P0 = get_hll_global [ 'Bool' ], 'True'
    .return ($P0)
  false:
    $P0 = get_hll_global [ 'Bool' ], 'False'
    .return ($P0)
.end


=item item

A List in item context becomes an Array.

=cut

.namespace ['List']
.sub 'item' :method
    .tailcall self.'Array'()
.end

=item list

A List in list context returns itself.

=cut

.namespace ['List']
.sub 'list' :method
    .return (self)
.end

.namespace []
.sub 'list'
    .param pmc values          :slurpy
    .tailcall values.'!flatten'()
.end

=back

=head2 Coercion methods

=over

=item Iterator

=cut

.namespace ['List']
.sub 'Iterator' :method
    self.'!flatten'()
    $P0 = iter self
    .return ($P0)
.end


=item Scalar

A list in Scalar context becomes a Scalar containing an Array.

=cut

.sub 'Scalar' :method
    $P0 = self.'Array'()
    $P0 = new 'Perl6Scalar', $P0
    .return ($P0)
.end

# FIXME:  :vtable('get_string') is wrong here.
.sub 'Str' :method :vtable('get_string')
    self.'!flatten'()
    $S0 = join ' ', self
    .return ($S0)
.end

=item ResizablePMCArray.list

This version of list morphs a ResizablePMCArray into a List.

=cut

.namespace ['ResizablePMCArray']
.sub 'list' :method
    ##  this code morphs a ResizablePMCArray into a List
    ##  without causing a clone of any of the elements
    $P0 = new 'ResizablePMCArray'
    splice $P0, self, 0, 0
    $P1 = new 'List'
    copy self, $P1
    splice self, $P0, 0, 0
    .return (self)
.end


=back

=head2 Methods

=over

=item elems()

Return the number of elements in the list.

=cut

.namespace ['List']
.sub 'elems' :method :multi('ResizablePMCArray') :vtable('get_number')
    self.'!flatten'()
    $I0 = elements self
    .return ($I0)
.end


.namespace ['List']
.sub 'reverse' :method
    .local pmc result, it
    result = new 'List'
    it = self.'iterator'()
  loop:
    unless it goto done
    $P0 = shift it
    unshift result, $P0
    goto loop
  done:
    .return (result)
.end


=back

=head2 Private methods

=over 4

=item !flatten()

Flatten the invocant, as in list context.  This doesn't necessarily
make the list eager, it just brings any nested Lists to the top
layer.  It will likely change substantially when we have lazy lists.

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
    $I0 = isa elem, 'Perl6Scalar'
    if $I0 goto flat_next
    $I0 = can elem, '!flatten'
    if $I0 goto flat_elem
    $I0 = does elem, 'array'
    unless $I0 goto flat_next
    splice self, elem, i, 1
    len = elements self
    goto flat_loop
  flat_next:
    inc i
    goto flat_loop
  flat_elem:
    elem = elem.'!flatten'()
    splice self, elem, i, 1
    $I0 = elements elem
    i += $I0
    len = elements self
    goto flat_loop
  flat_end:
    $I0 = isa self, 'List'
    if $I0 goto end
    self.'list'()
  end:
    .return (self)
.end


=item fmt

 our Str multi List::fmt ( Str $format, $separator = ' ' )

Returns the invocant list formatted by an implicit call to C<sprintf> on each
of the elements, then joined with spaces or an explicitly given separator.

=cut

.sub 'fmt' :method :multi('ResizablePMCArray')
    .param pmc format
    .param string sep          :optional
    .param int has_sep         :opt_flag

    .local pmc res
    .local pmc iter
    .local pmc retv
    .local pmc elem
    .local pmc elemres

    if has_sep goto have_sep
    sep = ' '
  have_sep:
    res = new 'List'
    iter = self.'iterator'()
  elem_loop:
    unless iter goto done

  invoke:
    elem = shift iter
    elemres = 'sprintf'(format, elem)
    push res, elemres
    goto elem_loop

  done:
    retv = 'join'(sep, res)
    .return(retv)
.end

=item iterator()

Returns an iterator for the list.

=cut

.sub 'iterator' :method
    self.'!flatten'()
    $P0 = iter self
    .return ($P0)
.end


=item uniq(...)

=cut

# TODO Rewrite it. It's too naive.

.namespace ['List']
.sub 'uniq' :method
    .param pmc comparer :optional
    .param int has_comparer :opt_flag

    if has_comparer goto have_comparer
    comparer = get_hll_global 'infix:eq'
  have_comparer:

    .local pmc ulist
    $P0 = get_hll_global 'List'
    ulist = $P0.'new'()

    .local pmc it_inner, it_outer, val
    it_outer = iter self
  outer_loop:
    unless it_outer goto outer_done
    val = shift it_outer
    it_inner = iter ulist
  inner_loop:
    unless it_inner goto inner_done
    $P0 = shift it_inner
    $P1 = comparer(val, $P0)
    if $P1 goto outer_loop
    goto inner_loop
  inner_done:
    ulist.'push'(val)
    goto outer_loop

  outer_done:
    .return (ulist)
.end


.namespace []
.sub 'uniq' :multi('Block')
    .param pmc comparer
    .param pmc values :slurpy
    values.'!flatten'()
    .tailcall values.'uniq'(comparer)
.end

.sub 'uniq' :multi()
    .param pmc values :slurpy
    values.'!flatten'()
    .tailcall values.'uniq'()
.end


=back

=head1 Functions

=over 4

=item C<infix:,(...)>

Operator form for building a list from its arguments.

=cut

.namespace []
.sub 'infix:,'
    .param pmc args            :slurpy
    .tailcall args.'list'()
.end


=item C<infix:Z(...)>

The zip operator.

=cut

.sub 'infix:Z'
    .param pmc arglist :slurpy
    .local pmc result

    # create a list to hold the results
    result = new 'List'

    unless arglist goto result_done

    # create a set of iterators, one per argument
    .local pmc iterlist, arglist_it
    iterlist = new 'ResizablePMCArray'
    arglist_it = iter arglist
  arglist_loop:
    unless arglist_it goto arglist_done
    .local pmc arg, arg_it
    arg = shift arglist_it
    arg_it = iter arg
    push iterlist, arg_it
    goto arglist_loop
  arglist_done:

    # repeatedly loop through the argument iterators in parallel,
    # building result elements as we go.  When we reach
    # an argument iterator with no more elements, we're done.

  outer_loop:
    .local pmc iterlist_it, reselem
    iterlist_it = iter iterlist
    reselem = new 'List'
  iterlist_loop:
    unless iterlist_it goto iterlist_done
    arg_it = shift iterlist_it
    unless arg_it goto result_done
    $P0 = shift arg_it
    reselem.'push'($P0)
    goto iterlist_loop
  iterlist_done:
    result.'push'(reselem)
    goto outer_loop

  result_done:
    .return (result)
.end


=item C<infix:X(...)>

The non-hyper cross operator.

=cut

.sub 'infix:X'
    .param pmc args            :slurpy
    .local pmc res

    .local pmc res, outer, inner, it, val
    res = new 'List'

    ##  if the are no arguments, result is empty list
    unless args goto done

    ##  get the first arg in list context
    outer = shift args
    outer = 'list'(outer)

    ##  if this argument is empty, result is empty list
    unless outer goto done

    ##  if no more args, then build result from only arg
    unless args goto one_arg

    ##  There are more args, so recursively compute their cross.
    ##  If that list is empty, our cross is empty.
    inner = 'infix:X'(args :flat)
    unless inner goto done

    ##  otherwise, loop through all elements of our first arg
    it = iter outer
  outer_loop:
    unless it goto done
    val = shift it
    ##  add the value to a clone of each inner result list
    $P1 = iter inner
  inner_loop:
    unless $P1 goto outer_loop
    ##  get a result list, clone it
    $P0 = shift $P1
    $P0 = clone $P0
    ##  add our outer value to the beginning
    unshift $P0, val
    ##  save it in the result list
    push res, $P0
    goto inner_loop

    ##  if call to infix:X had only one argument, our result
    ##  is a list of 1-element lists.
  one_arg:
    it = iter outer
  one_arg_loop:
    unless it goto done
    val = shift it
    $P0 = new 'List'
    push $P0, val
    push res, $P0
    goto one_arg_loop

  done:
    .return (res)
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
    .local pmc cur_min, it
    cur_min = args[0]
    it = iter args
find_min_loop:
    unless it goto find_min_loop_end
    $P0 = shift it
    $I0 = 'infix:cmp'($P0, cur_min)
    unless $I0 < 0 goto find_min_loop
    set cur_min, $P0
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
    .local pmc cur_max, it
    cur_max = args[0]
    it = iter args
find_max_loop:
    unless it goto find_max_loop_end
    $P0 = shift it
    $I0 = 'infix:cmp'($P0, cur_max)
    unless $I0 > 0 goto find_max_loop
    set cur_max, $P0
    goto find_max_loop
find_max_loop_end:

    .return(cur_max)
.end

## TODO: zip

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
