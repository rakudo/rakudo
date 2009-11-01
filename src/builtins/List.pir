=head1 TITLE

List - Perl 6 List class

=head1 DESCRIPTION

This file implements Lists, which are immutable sequences
of objects.  Lists are also lazy, which means that some
items are not generated until they are needed.

=head2 Methods

=over 4

=cut

.namespace ['List']
.sub 'onload' :anon :init :load
    .local pmc p6meta, listproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    listproto = p6meta.'new_class'('List', 'parent'=>'Any', 'attr'=>'$!values $!gen')
.end

=item elems()

Return the number of elements in the list.

=cut

.sub 'elems' :method
    .local pmc values
    values = self.'!generate'()
    $I0 = values
    .return ($I0)
.end

=item Int()

=cut

.sub 'Int' :method
    .tailcall self.'elems'()
.end

=item list()

=cut

.sub 'list' :method
    .return (self)
.end

=item Num()

=cut

.sub 'Num' :method
    .tailcall self.'elems'()
.end

=item postcircumfix:<[ ]>(Int)

=cut

.sub 'postcircumfix:<[ ]>' :method :multi(_, ['Integer']) 
    .param int n
    .local pmc values, elem
    if n < 0 goto fail
    values = self.'!generate'(n)
    elem = values[n]
    if null elem goto fail
    .return (elem)
  fail:
    .tailcall '!FAIL'('Index beyond end of List')
.end

=back

=head2 Private methods

=over 4

=item !generator(int n)

Ask the list to make at least the first C<n> elements as non-lazy
as it can, then return its entire list of elements as a 
ResizablePMCArray or Parcel.  If C<n> isn't given, then eagerly
generate the entire list.

=cut

.sub '!generate' :method
    .param int n               :optional
    .param int has_n           :opt_flag
    .local pmc values, gen
    values = getattribute self, '$!values'
    gen    = getattribute self, '$!gen'
  gen_loop:
    .local int gen_i, values_i
    gen_i = gen
    values_i = elements values
    if has_n goto gen_elem
    n = values_i
  gen_elem:
    unless gen_i < n goto gen_done
    if gen_i >= values_i goto gen_done
    .local pmc elem
    elem = values[gen_i]
    $P0 = getprop 'flatten', elem
    if null $P0 goto gen_next
    $I0 = n - gen_i
    $P0 = elem.'!generate'($I0)
    splice values, $P0, gen_i, 1
    goto gen_loop
  gen_next:
    inc gen
    goto gen_loop
  gen_done:
    .return (values)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
