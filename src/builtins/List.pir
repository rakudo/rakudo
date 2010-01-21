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

=item new(*@args)

Setup initializers for built in variables.

=cut

.sub 'new' :method
    .param pmc args :slurpy
    .local pmc candidate, true
    candidate = self.'CREATE'('P6opaque')
    true = get_hll_global ['Bool'], 'True'
    setprop candidate, 'flatten', true
    .tailcall self.'bless'(candidate, 'values'=>args, 'gen'=>0 )
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
    .local pmc list, gen, true
    list = new ['List']
    setattribute list, '$!values', args
    gen = box 0
    setattribute list, '$!gen', gen
    true = get_hll_global ['Bool'], 'True'
    setprop list, 'flatten', true
    .return (list)
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
generate the entire list.  Any RPAs (incl. Parcels) in $!values
always flatten here, even if not marked with the 'flatten' flag.

=cut

.namespace ['List']
.sub '!generate' :method
    .param int n               :optional
    .param int has_n           :opt_flag
    .local pmc values
    values = getattribute self, '$!values'
    unless null values goto have_values
    values = new ['ResizablePMCArray']
    setattribute self, '$!values', values
  have_values:
    .local pmc gen
    gen = getattribute self, '$!gen'
    unless null gen goto have_gen
    gen = box 0
    setattribute self, '$!gen', gen
  have_gen:
    .local int gen_i
    gen_i = gen
  gen_loop:
    $I0 = elements values
    if gen_i >= $I0 goto gen_done
    .local pmc elem
    elem = values[gen_i]
    if null elem goto gen_next
    # RPAs (incl Parcel) always flatten
    $I1 = isa elem, ['ResizablePMCArray']
    if $I1 goto have_flat_elem
    # see if we have a flattening element
    $P0 = getprop 'flatten', elem
    if null $P0 goto gen_next
    # see if we need to flatten a certain number of elems
    if has_n goto gen_elem_n
    elem = elem.'!generate'()
    goto have_flat_elem
  gen_elem_n:
    $I0 = n - gen_i
    elem = elem.'!generate'($I0)
  have_flat_elem:
    splice values, elem, gen_i, 1
    goto gen_loop
  gen_next:
    inc gen_i
    unless has_n goto gen_loop
    if gen_i < n goto gen_loop
  gen_done:
    gen = gen_i
    .return (values)
.end

=item !STORE(source)

Performs list assignment using the values from C<source>.

=cut

.namespace ['List']
.sub '!STORE' :method
    .param pmc source

    # First, create a flattening Array from C<source>.  This 
    # creates # copies of values in C<source>, in case any lvalue target
    # containers in C<self> also happen to be listed as rvalues
    # in C<source>.  (Creating a copy of everything in C<source>
    # likely isn't the most efficient approach to this, but it 
    # works for now.)
    source = '&circumfix:<[ ]>'(source)
    $P0 = get_hll_global ['Bool'], 'True'
    setprop source, 'flatten', $P0
   

    # Now, loop through targets of C<self>, storing the corresponding
    # values from source and flattening any RPAs we encounter.  If a 
    # target is an array or hash, then it will end up consuming all 
    # of the remaining elements of source.
    .local pmc targets
    targets = getattribute self, '$!values'
    targets = clone targets
  store_loop:
    unless targets goto store_done
    .local pmc cont
    cont = shift targets
    $I0 = isa cont, ['ResizablePMCArray']
    if $I0 goto store_rpa
    $I0 = isa cont, ['Perl6Scalar']
    if $I0 goto store_scalar
    $I0 = isa cont, ['Array']
    if $I0 goto store_array
  store_scalar:
    $P0 = shift source
    cont.'!STORE'($P0)
    goto store_loop
  store_array:
    cont.'!STORE'(source)
    source = '&circumfix:<[ ]>'()
    goto store_loop
  store_rpa:
    splice targets, cont, 0, 0
    goto store_loop
  store_done:

    .return (self)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
