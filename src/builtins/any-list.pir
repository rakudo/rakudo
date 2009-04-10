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
    sep = ''
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
.sub 'keys' :multi() :subid('_keys')
    .param pmc values          :slurpy
    values.'!flatten'()
    .tailcall values.'keys'()
.end
.sub '' :init :load
    .local pmc block, signature
    .const 'Sub' $P0 = "_keys"
    block = $P0
    signature = new ["Signature"]
    setprop block, "$!signature", signature
    signature.'!add_param'('@values', 1 :named('slurpy'))
    '!TOPERL6MULTISUB'(block)
.end


.namespace ['Any']
.sub 'keys' :method :subid('any_keys')
    $I0 = self.'elems'()
    $P0 = 'prefix:^'($I0)
    .tailcall $P0.'list'()
.end
.sub '' :init :load
    .local pmc block, signature
    .const 'Sub' $P0 = "any_keys"
    block = $P0
    signature = new ["Signature"]
    setprop block, "$!signature", signature
    $P0 = get_hll_global 'Any'
    signature."!add_implicit_self"($P0)
.end


=item kv

=cut

.namespace []
.sub 'kv' :multi() :subid('_kv')
    .param pmc values          :slurpy
    values.'!flatten'()
    .tailcall values.'kv'()
.end
.sub '' :init :load
    .local pmc block, signature
    .const 'Sub' $P0 = "_kv"
    block = $P0
    signature = new ["Signature"]
    setprop block, "$!signature", signature
    signature.'!add_param'('@values', 1 :named('slurpy'))
    '!TOPERL6MULTISUB'(block)
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
.sub 'values' :multi() :subid('_values')
    .param pmc values          :slurpy
    .tailcall values.'!flatten'()
.end
.sub '' :init :load
    .local pmc block, signature
    .const 'Sub' $P0 = "_values"
    block = $P0
    signature = new ["Signature"]
    setprop block, "$!signature", signature
    signature.'!add_param'('@values', 1 :named('slurpy'))
    '!TOPERL6MULTISUB'(block)
.end

.namespace ['Any']
.sub 'values' :method
    self.'list'()
    .return (self)
.end


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:

