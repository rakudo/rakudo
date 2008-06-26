## $Id$

=head1 TITLE

Any - Perl 6 Any class

=head1 DESCRIPTION

This file implements the Any class.

=head2 Basic C<Any> methods

=over 4

=cut

.namespace []
.sub 'onload' :anon :init :load
    .local pmc p6meta
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    p6meta.'new_class'('Any', 'parent'=>'Perl6Object')

    ##  pre-seed a random number generator
    $P0 = new 'Random'
    set_hll_global ['Any'], '$!random', $P0
    srand()
.end

.namespace ['Any']

=item can($x)

=cut

.sub 'can' :method
    .param pmc x
    $P0 = self.'HOW'()
    .return $P0.'can'(x)
.end

=item isa($x)

=cut

.sub 'isa' :method
    .param pmc x
    $P0 = self.'HOW'()
    .return $P0.'isa'(x)
.end

=back

=head2 C<Num>-like functions and methods

=over 4

=item rand()

=cut

.namespace []
.sub 'rand'
    .param pmc x               :slurpy
    ## 0-argument test, RT#56366
    unless x goto no_args
    die "too many arguments passed - 0 params expected"
  no_args:
    $P0 = get_hll_global ['Any'], '$!random'
    $N0 = $P0
    .return ($N0)
.end

.namespace ['Any']
.sub 'rand' :method
    $N0 = self
    $P0 = get_hll_global ['Any'], '$!random'
    $N1 = $P0
    $N0 *= $N1
    .return ($N0)
.end

=item srand()

=cut

.namespace []
.sub 'srand'
    .param num seed            :optional
    .param int has_seed        :opt_flag
    if has_seed goto have_seed
    seed = time
  have_seed:
    $P0 = get_hll_global ['Any'], '$!random'
    $I0 = seed
    $P0 = $I0
    .return ()
.end

.namespace ['Any']
.sub 'srand' :method
    $N0 = self
    $I0 = $N0
    $P0 = get_hll_global ['Any'], '$!random'
    $P0 = $I0
    .return ()
.end

=back

=head2 C<List>-like functions and methods

=over 4

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

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
