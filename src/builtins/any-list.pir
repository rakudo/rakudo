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
    '!EXPORT'('abs', 'from'=>$P0)
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
    sep = ' '
  have_sep:
    $P0 = self.'list'()
    $P0.'!flatten'()
    $S0 = join sep, $P0
    .return ($S0)
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
    result = 'undef'()
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
    result = 'undef'()
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
    by = get_hll_global 'infix:cmp'
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
    by = get_hll_global 'infix:cmp'
  have_by:

    ##  prepare self and FPA for sorting
    .local pmc list, fpa
    .local int elems
    list = self.'list'()
    elems = list.'elems'()
    fpa = new 'FixedPMCArray'
    assign fpa, elems
    $I0 = by.'arity'()
    if $I0 < 2 goto by_value_cmp

    ##  normal compare function, build fpa from list
    .local pmc it
    elems = 0
    it = iter list
  fpa_loop:
    unless it goto fpa_done
    $P0 = shift it
    fpa[elems] = $P0
    inc elems
    goto fpa_loop
  fpa_done:
    fpa.'sort'(by)
    .tailcall 'list'(fpa)

  by_value_cmp:
    ##  Algorithm as Perl 6:
    ##      my @v     = @list.map($by);
    ##      my @slice = (0..^@list).sort: { @v[$^a] cmp @v[$^b]};
    ##      return @list[ @slice ];

    .local pmc values
    values = list.'map'(by)
    set_global '@!sort_values', values
    ##  fill fpa with values 0..elems-1
    $I0 = 0
  fpa_range_loop:
    unless $I0 < elems goto fpa_range_done
    fpa[$I0] = $I0
    inc $I0
    goto fpa_range_loop
  fpa_range_done:
    .const 'Sub' sbv = '!sort_by_value'
    fpa.'sort'(sbv)
    ##  return sorted slice of original list
    .tailcall list.'postcircumfix:[ ]'(fpa)
.end

.sub '!sort_by_value' :anon
    .param pmc a
    .param pmc b
    .local pmc values
    values = get_global '@!sort_values'
    $P0 = values[a]
    $P1 = values[b]
    $I0 = 'infix:cmp'($P0, $P1)
    .return ($I0)
.end
    
=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:

