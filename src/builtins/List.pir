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
    .local pmc p6meta, listproto, pos_role
    p6meta = get_hll_global ['Mu'], '$!P6META'
    
    # Select generic version of Positional (for untyped) and do it.
    # XXX When List becomes a parametric role too, we'd pass in the
    # given T to select below.
    pos_role = get_hll_global 'Positional'
    pos_role = pos_role.'!select'()

    # Create the class.
    listproto = p6meta.'new_class'('List', 'parent'=>'Any', 'attr'=>'$!values $!gen', 'does_role'=>pos_role)
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

=item Iterator()

=cut

.sub 'Iterator' :method
    .local pmc list_it, index
    list_it = new ['ListIterator']
    index = box 0
    setattribute list_it, '$!list', self
    setattribute list_it, '$!index', index
    .return (list_it)
.end

=item list()

=cut

.sub 'list' :method
    .return (self)
.end

=item postcircumfix:<[ ]>(Int)

=cut

.sub 'postcircumfix:<[ ]>' :method :multi(_, ['Integer']) 
    .param int n
    .local pmc values, elem
    if n < 0 goto fail
    $I0 = n + 1
    values = self.'!generate'($I0)
    elem = values[n]
    if null elem goto fail
    .return (elem)
  fail:
    .tailcall '!FAIL'('Index beyond end of List')
.end

=head2 Functions

=over 4

=item &list(...)

Creates a List from its arguments.

=cut

.namespace []
.sub '&list'
    .param pmc args            :slurpy
    $P0 = new ['List']
    setattribute $P0, '$!values', args
    .return ($P0)
.end


=back

=head2 Private methods

=over 4

=item !FETCH()

=cut

.namespace ['List']
.sub '!FETCH' :method
    $P0 = root_new ['parrot';'Perl6Scalar'], self
    .return ($P0)
.end

=item !generator(int n)

Ask the list to make at least the first C<n> elements as non-lazy
as it can, then return its entire list of elements as a 
ResizablePMCArray or Parcel.  If C<n> isn't given, then eagerly
generate the entire list.

=cut

.namespace ['List']
.sub '!generate' :method
    .param int n               :optional
    .param int has_n           :opt_flag
    .local pmc values, gen
    values = getattribute self, '$!values'
    unless null values goto have_values
    values = new ['ResizablePMCArray']
    setattribute self, '$!values', values
  have_values:
    gen    = getattribute self, '$!gen'
    unless null gen goto have_gen
    gen = box 0
    setattribute self, '$!gen', gen
  have_gen:
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
    if null elem goto gen_next
    $P0 = getprop 'flatten', elem
    if null $P0 goto gen_next
    $I0 = n - gen_i
    $I1 = isa elem, ['ResizablePMCArray']
    if $I1 goto have_flat_elem
    elem = elem.'!generate'($I0)
  have_flat_elem:
    splice values, elem, gen_i, 1
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
