## $Id$

=head1 NAME

src/builtins/any-list.pir -  C<List>-like functions and methods for C<Any>

=head1 DESCRIPTION

This file implements the methods and functions of C<Any> that
are most closely associated with the C<List> class or role.
We place them here instead of F<src/classes/Any.pir> to keep
the size of that file down and to emphasize their generic,
"built-in" nature.

=head2 Methods

=over 4

=cut

.namespace ['Any']
.sub 'onload' :anon :init :load
    $P0 = get_hll_namespace ['Any']
    '!EXPORT'('end', 'from'=>$P0)
.end

=item elems()

=cut

.namespace []
.sub 'elems' :multi()
    .param pmc values          :slurpy
    $P0 = values.'!flatten'()
    .tailcall values.'elems'()
.end

.namespace ['Any']
.sub 'elems' :method :vtable('elements') :multi(_)
    $P0 = self.'list'()
    $I0 = $P0.'elems'()
    .return ($I0)
.end

=item end

=cut

.namespace ['Any']
.sub 'end' :method :multi(_)
    .local pmc list
    list = self.'list'()
    $I0 = list.'elems'()
    dec $I0
    .return ($I0)
.end

=item first(...)

=cut

.namespace []
.sub 'first' :multi('Sub')
    .param pmc test
    .param pmc values :slurpy

    .tailcall values.'first'(test)
.end

.namespace ['Any']
.sub 'first' :method :multi(_, 'Sub')
    .param pmc test
    .local pmc retv
    .local pmc iter
    .local pmc block_res
    .local pmc block_arg

    iter = self.'iterator'()
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
    retv = '!FAIL'('Undefined value - first list match of no matches')

  done:
    .return(retv)
.end

=item grep(...)

=cut

.namespace []
.sub 'grep' :multi('Sub')
    .param pmc test
    .param pmc values          :slurpy
    .tailcall values.'grep'(test)
.end

.namespace ['Any']
.sub 'grep' :method :multi(_, 'Sub')
    .param pmc test
    .local pmc retv
    .local pmc iter
    .local pmc block_res
    .local pmc block_arg

    retv = new 'List'
    iter = self.'iterator'()
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

=item join

=cut

.namespace []
.sub 'join' :multi('String')
    .param string sep
    .param pmc values          :slurpy
    .tailcall values.'join'(sep)
.end

.namespace ['Any']
.sub 'join' :method :multi(_)
    .param string sep          :optional
    .param int has_sep         :opt_flag
    if has_sep goto have_sep
    sep = ' '
  have_sep:
    $P0 = self.'list'()
    $P0.'!flatten'()
    $S0 = join sep, $P0
    .return ($S0)
.end

=item keys()

Return a List with the keys of the invocant.

=cut

.namespace []
.sub 'keys' :multi()
    .param pmc values          :slurpy
    values.'!flatten'()
    .tailcall values.'keys'()
.end

.namespace ['Any']
.sub 'keys' :method
    $I0 = self.'elems'()
    $P0 = 'infix:^'($I0)
    .tailcall $P0.'list'()
.end


=item kv

=cut

.namespace []
.sub 'kv' :multi()
    .param pmc values          :slurpy
    values.'!flatten'()
    .tailcall values.'kv'()
.end
    
.namespace ['Any']
.sub 'kv' :method
    .local pmc result, it
    result = new 'List'
    it = self.'iterator'()
    .local int i
    i = 0
  loop:
    unless it goto done
    $P0 = shift it
    push result, i
    push result, $P0
    inc i
    goto loop
  done:
    .return (result)
.end


=item map()

=cut

.namespace []
.sub 'map' :multi('Sub')
    .param pmc expression
    .param pmc values          :slurpy
    .tailcall values.'map'(expression)
.end

.namespace ['Any']
.sub 'map' :method :multi(_, 'Sub')
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

=item min

=cut

.namespace []
.sub 'min' :multi()
    .param pmc values          :slurpy
    .local pmc by
    by = get_hll_global 'infix:cmp'
    unless values goto have_by
    $P0 = values[0]
    $I0 = isa $P0, 'Sub'
    unless $I0 goto have_by
    by = shift values
  have_by:
    .tailcall values.'min'(by)
.end


.namespace ['Any']
.sub 'min' :method :multi(_)
    .param pmc by              :optional
    .param int has_by          :opt_flag
    if has_by goto have_by
    by = get_hll_global 'infix:cmp'
  have_by:

    .local pmc it, result
    $P0 = self.'list'()
    it = $P0.'iterator'()
    unless it goto fail
    result = shift it
  loop:
    unless it goto done
    $P0 = shift it
    $I0 = by($P0, result)
    unless $I0 < 0 goto loop
    result = $P0
    goto loop
  fail:
    .local num failres
    failres = "+Inf"
    .return (failres)
  done:
    .return (result)
.end


.namespace []
.sub 'max' :multi()
    .param pmc values          :slurpy
    .local pmc by
    by = get_hll_global 'infix:cmp'
    unless values goto have_by
    $P0 = values[0]
    $I0 = isa $P0, 'Sub'
    unless $I0 goto have_by
    by = shift values
  have_by:
    .tailcall values.'max'(by)
.end


.namespace ['Any']
.sub 'max' :method :multi(_)
    .param pmc by              :optional
    .param int has_by          :opt_flag
    if has_by goto have_by
    by = get_hll_global 'infix:cmp'
  have_by:

    .local pmc it, result
    $P0 = self.'list'()
    it = $P0.'iterator'()
    unless it goto fail
    result = shift it
  loop:
    unless it goto done
    $P0 = shift it
    $I0 = by($P0, result)
    unless $I0 > 0 goto loop
    result = $P0
    goto loop
  fail:
    .local num failres
    failres = "-Inf"
    .return (failres)
  done:
    .return (result)
.end


=item pairs()

=cut

.namespace []
.sub 'pairs' :multi()
    .param pmc values          :slurpy
    values.'!flatten'()
    .tailcall values.'pairs'()
.end

.namespace ['Any']
.sub 'pairs' :method
    .local pmc result, it
    result = new 'List'
    it = self.'iterator'()
    .local int i
    i = 0
  loop:
    unless it goto done
    $P0 = shift it
    $P1 = 'infix:=>'(i, $P0)
    push result, $P1
    inc i
    goto loop
  done:
    .return (result)
.end


=item pick($num, :$repl)

=cut

.namespace []
.sub 'pick' :multi(_)
    .param int p_num
    .param pmc values          :slurpy
    .param pmc p_repl          :optional :named('repl')
    .param int has_repl        :opt_flag
    if has_repl goto have_repl
    p_repl = get_hll_global ['Bool'], 'False'
  have_repl:
    .tailcall values.'pick'(p_num, 'repl'=>p_repl)
.end

.sub 'pick' :multi('Whatever')
    .param pmc whatever
    .param pmc values          :slurpy
    .param pmc p_repl          :optional :named('repl')
    .param int has_repl        :opt_flag
    unless has_repl goto no_repl
    unless p_repl goto no_repl
    die "Infinite lazy pick not implemented"
  no_repl:
    .tailcall values.'pick'(whatever)
.end

.namespace ['Any']
.sub 'pick' :method :multi()
    .param int p_num           :optional
    .param int has_num         :opt_flag
    .param pmc p_repl          :optional :named('repl')
    .param int has_repl        :opt_flag

    .local pmc list, result, rand
    .local int elems
    list = self.'list'()
    elems = list.'elems'()
    result = 'list'()
    rand = get_hll_global ['Any'], '$!random'

    if has_num goto have_num
    p_num = 1
  have_num:

    .local int repl
    repl = 0
    unless has_repl goto have_repl
    repl = istrue p_repl
  have_repl:
    if repl goto skip_clone
    list = clone list
  skip_clone:

  loop:
    unless p_num > 0 goto done
    unless elems > 0 goto done
    $N0 = rand
    $N0 *= elems
    $I0 = $N0
    $P0 = list[$I0]
    push result, $P0
    dec p_num
    if repl goto loop
    delete list[$I0]
    elems = list.'elems'()
    goto loop
  done:
    .return (result)
.end

.sub 'pick' :method :multi(_, 'Whatever')
    .param pmc whatever
    .param pmc p_repl          :optional :named('repl')
    .param int has_repl        :opt_flag
    unless has_repl goto no_repl
    unless p_repl goto no_repl
    die "Infinite lazy pick not implemented"
  no_repl:
    $I0 = self.'elems'()
    .tailcall self.'pick'($I0)
.end

=item reduce(...)

=cut

.namespace []
.sub 'reduce' :multi('Sub')
    .param pmc expression
    .param pmc values          :slurpy
    .tailcall values.'reduce'(expression)
.end

.namespace ['Any']
.sub 'reduce' :method :multi(_, 'Sub')
    .param pmc expression
    .local pmc retv
    .local pmc iter
    .local pmc elem
    .local pmc args
    .local int i, arity

    arity = expression.'arity'()
    if arity < 2 goto error

    iter = self.'iterator'()
    unless iter goto empty
    retv = shift iter
  loop:
    unless iter goto done

    # Create arguments for closure
    args = new 'ResizablePMCArray'
    # Start with 1. First argument is result of previous call
    i = 1

  args_loop:
    if i == arity goto invoke
    unless iter goto elem_undef
    elem = shift iter
    goto push_elem
  elem_undef:
    elem = 'undef'()

  push_elem:
    push args, elem
    inc i
    goto args_loop

  invoke:
    retv = expression(retv, args :flat)
    goto loop

  empty:
    .tailcall '!FAIL'('Cannot reduce an empty list')

  error:
    'die'('Cannot reduce() using a unary or nullary function.')

  done:
    .return(retv)
.end

=item reverse()

=cut

.namespace ['Any']
.sub 'reverse' :method :multi(_)
    .local pmc result, it
    result = new 'List'
    $P0 = self.'list'()
    it = $P0.'iterator'()
  loop:
    unless it goto done
    $P0 = shift it
    unshift result, $P0
    goto loop
  done:
    .return (result)
.end

.namespace []
.sub 'reverse' :multi()
    .param pmc values          :slurpy
    .tailcall values.'reverse'()
.end


=item sort()

Sort list.  In this case we copy into an FPA to make use of the
Parrot's built-in sort algorithm.

=cut

.namespace []
.sub 'sort' :multi()
    .param pmc values          :slurpy
    .local pmc by
    by = find_name 'infix:cmp'
    unless values goto have_by
    $P0 = values[0]
    $I0 = isa $P0, 'Sub'
    unless $I0 goto have_by
    by = shift values
  have_by:
    .tailcall values.'sort'(by)
.end

.namespace ['Any']
.sub 'sort' :method :multi(_)
    .param pmc by              :optional
    .param int has_by          :opt_flag
    if has_by goto have_by
    by = find_name 'infix:cmp'
  have_by:

    ##  prepare self for sorting
    .local pmc list
    .local int elems
    list = self.'list'()
    elems = list.'elems'()
    ##  If there are fewer than two elements, no need to sort.
    unless elems < 2 goto do_sort
    .return (list)

  do_sort:
    ##  Get the comparison function to use.  We don't use C<by>
    ##  directly, because FPA's sort doesn't work with MultiSub
    ##  functions and isn't stable.  !COMPARESUB expects to be
    ##  sorting indexes into C<list>, and also handles generation
    ##  of values for subs with arity < 2.
    .local pmc cmp
    cmp = '!COMPARESUB'(list, by)

    ##  create a FPA of indexes to be sorted using cmp
    .local pmc fpa
    fpa = new 'FixedPMCArray'
    assign fpa, elems
    $I0 = 0
  fpa_loop:
    unless $I0 < elems goto fpa_done
    fpa[$I0] = $I0
    inc $I0
    goto fpa_loop
  fpa_done:
    fpa.'sort'(cmp)
    .tailcall list.'postcircumfix:[ ]'(fpa)
.end

.sub '!COMPARESUB' :anon
    .param pmc list
    .param pmc by
    $I0 = can by, 'arity'
    unless $I0 goto have_list
    $I0 = by.'arity'()
    unless $I0 < 2 goto have_list
    list = list.'map'(by)
    by = find_name 'infix:cmp'
  have_list:
    ##  Because of TT #56, we can't store Sub PMCs directly into
    ##  the namespace.  So, we create an array to hold it for us.
    set_global '@!compare', list
    $P0 = new 'ResizablePMCArray'
    push $P0, by
    set_global '@!compare_by', $P0
    .const 'Sub' $P99 = '!COMPARE_DO'
    .return ($P99)
.end

.sub '!COMPARE_DO' :anon
    .param int a
    .param int b
    .local pmc list, by
    list = get_global '@!compare'
    $P0  = get_global '@!compare_by'
    by   = $P0[0]

    $P0 = list[a]
    $P1 = list[b]
    $I0 = by($P0, $P1)
    unless $I0 == 0 goto done
    $I0 = cmp a, b
  done:
    .return ($I0)
.end


=item values

Return values of the list

=cut

.namespace []
.sub 'values' :multi()
    .param pmc values          :slurpy
    .tailcall values.'!flatten'()
.end

.namespace ['Any']
.sub 'values' :method
    self.'!flatten'()
    .return (self)
.end


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:

