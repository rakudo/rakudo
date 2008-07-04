## $Id$

=head1 NAME

src/builtins/any_list.pir -  C<List>-like functions and methods for C<Any>

=head1 DESCRIPTION

This file implements the methods and functions of C<Any> that
are most closely associated with the C<Num> class or role.
We place them here instead of L<src/classes/Any.pir> to keep
the size of that file down and to emphasize their generic,
"built-in" nature.

=head2 Methods

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
    .return values.'elems'()
.end

.namespace ['Any']
.sub 'elems' :method :vtable('elements') :multi(_)
    $P0 = self.'list'()
    $I0 = $P0.'elems'()
    .return ($I0)
.end


=item join

=cut

.namespace []
.sub 'join' :multi('String')
    .param string sep
    .param pmc values          :slurpy
    .return values.'join'(sep)
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
    .return values.'min'(by)
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
    .return values.'max'(by)
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
    .return values.'pick'(p_num, 'repl'=>p_repl)
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
    .return values.'pick'(whatever)
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
    .return self.'pick'($I0)
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
    .return values.'sort'(by)
.end

.namespace ['Any']
.sub 'sort' :method :multi(_)
    .param pmc by              :optional
    .param int has_by          :opt_flag
    if has_by goto have_by
    by = get_hll_global 'infix:cmp'
  have_by:

    .local pmc list, fpa
    .local int elems

    list = self.'list'()
    list.'!flatten'()
    elems = list.'elems'()
    fpa = new 'FixedPMCArray'
    fpa = elems

    .local int i
    i = 0
  fpa_loop:
    unless i < elems goto fpa_end
    $P0 = list[i]
    fpa[i] = $P0
    inc i
    goto fpa_loop
  fpa_end:
    fpa.'sort'(by)
    .return 'list'(fpa)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:

