## $Id$

=head1 NAME

src/classes/Array.pir - Perl 6 Array class and related functions

=cut

.namespace []
.sub '' :anon :load :init
    .local pmc p6meta, arrayproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    arrayproto = p6meta.'new_class'('Perl6Array', 'parent'=>'List', 'name'=>'Array')
    arrayproto.'!MUTABLE'()

    $P0 = get_hll_namespace ['Perl6Array']
    '!EXPORT'('exists,pop,push,shift,unshift', 'from'=>$P0, 'to_p6_multi'=>1)
.end


=head2 Methods

=item exists(indices :slurpy)

Return true if the elements at C<indices> have been assigned to.

=cut

.namespace ['Perl6Array']
.sub 'exists' :method :multi() :subid('array_exists')
    .param pmc indices :slurpy
    .local int test

    test = 0
  indices_loop:
    unless indices goto indices_end
    $I0 = shift indices
    test = exists self[$I0]
    if test goto indices_loop
  indices_end:
    .tailcall 'prefix:?'(test)
.end
.sub '' :init :load
    .local pmc block, signature
    .const 'Sub' $P0 = "array_exists"
    block = $P0
    signature = allocate_signature 2
    setprop block, "$!signature", signature
    null $P1
    $P0 = get_hll_global 'Array'
    set_signature_elem signature, 0, "self", SIG_ELEM_INVOCANT_AND_MULTI_INVOCANT, $P0, $P1, $P1, $P1, $P1, $P1
    set_signature_elem signature, 1, "@indices", SIG_ELEM_SLURPY_POS, $P1, $P1, $P1, $P1, $P1, $P1
.end


=item item()

Return Array in item context (i.e., self)

=cut

.namespace ['Perl6Array']
.sub 'item' :method
    .return (self)
.end


=item list

Return invocant as a List.

=cut

.namespace ['Perl6Array']
.sub '' :method('list')
    .tailcall self.'values'()
.end


=item pop()

Remove the last item from the array and return it.

=cut

.sub 'pop' :method :multi() :subid('array_pop')
    .local pmc x
    unless self goto empty
    x = pop self
    goto done
  empty:
    x = '!FAIL'('Undefined value popped from empty array')
  done:
    .return (x)
.end
.sub '' :init :load
    .local pmc block, signature
    .const 'Sub' $P0 = "array_pop"
    block = $P0
    signature = allocate_signature 1
    setprop block, "$!signature", signature
    $P0 = get_hll_global 'Array'
    null $P1
    set_signature_elem signature, 0, "self", SIG_ELEM_INVOCANT_AND_MULTI_INVOCANT, $P0, $P1, $P1, $P1, $P1, $P1
.end


=item push(args :slurpy)

Add C<args> to the end of the Array.

=cut

.sub 'push' :method :multi() :subid('array_push')
    .param pmc args :slurpy
    .local pmc type, it
    type = self.'of'()
    args.'!flatten'()
    $I1 = elements args
    $I0 = 0
  it_loop:
    if $I0 >= $I1 goto it_loop_end
    $P0 = new ['Perl6Scalar']
    setprop $P0, 'type', type
    $P1 = args[$I0]
    $P0.'!STORE'($P1, 'Push')
    args[$I0] = $P0
    inc $I0
    goto it_loop
  it_loop_end:
    $I0 = elements self
    splice self, args, $I0, 0
    .return (self)
.end
.sub '' :init :load
    .local pmc block, signature
    .const 'Sub' $P0 = "array_push"
    block = $P0
    signature = allocate_signature 2
    setprop block, "$!signature", signature
    null $P1
    $P0 = get_hll_global 'Array'
    set_signature_elem signature, 0, "self", SIG_ELEM_INVOCANT_AND_MULTI_INVOCANT, $P0, $P1, $P1, $P1, $P1, $P1
    set_signature_elem signature, 1, "@items", SIG_ELEM_SLURPY_POS, $P1, $P1, $P1, $P1, $P1, $P1
.end


=item shift()

Shift the first item off the array and return it.

=cut

.sub 'shift' :method :multi() :subid('array_shift')
    .local pmc x
    unless self goto empty
    x = shift self
    goto done
  empty:
    x = '!FAIL'('Undefined value shifted from empty array')
  done:
    .return (x)
.end
.sub '' :init :load
    .local pmc block, signature
    .const 'Sub' $P0 = "array_shift"
    block = $P0
    signature = allocate_signature 1
    setprop block, "$!signature", signature
    $P0 = get_hll_global 'Array'
    null $P1
    set_signature_elem signature, 0, "self", SIG_ELEM_INVOCANT_AND_MULTI_INVOCANT, $P0, $P1, $P1, $P1, $P1, $P1
.end


=item unshift(args :slurpy)

Adds C<args> to the beginning of the Array.

=cut

.sub 'unshift' :method :multi() :subid('array_unshift')
    .param pmc args :slurpy
    .local pmc type, it
    type = self.'of'()
    args.'!flatten'()
    it = iter args
  it_loop:
    unless it goto it_loop_end
    $P0 = shift it
    $I0 = type.'ACCEPTS'($P0)
    unless $I0 goto type_error
    goto it_loop
  it_loop_end:
    splice self, args, 0, 0
    .return (self)
  type_error:
    'die'('Type check failure in push')
.end
.sub '' :init :load
    .local pmc block, signature
    .const 'Sub' $P0 = "array_unshift"
    block = $P0
    signature = allocate_signature 2
    setprop block, "$!signature", signature
    null $P1
    $P0 = get_hll_global 'Array'
    set_signature_elem signature, 0, "self", SIG_ELEM_INVOCANT_AND_MULTI_INVOCANT, $P0, $P1, $P1, $P1, $P1, $P1
    set_signature_elem signature, 1, "@items", SIG_ELEM_SLURPY_POS, $P1, $P1, $P1, $P1, $P1, $P1
.end

=item values()

Return Array as a List of its values.

=cut

.namespace ['Perl6Array']
.sub 'values' :method
    $P0 = new ['List']
    splice $P0, self, 0, 0
    .return ($P0)
.end

=back

=head2 Operators

=over

=item circumfix:[]

Create an array.

=cut

.namespace []
.sub 'circumfix:[ ]'
    .param pmc values          :slurpy
    .tailcall values.'Scalar'()
.end


=back

=head2 Coercion methods

=over

=item Array

=cut

.namespace ['Perl6Array']
.sub 'Array' :method
    .return (self)
.end


=back

=head2 Private Methods

=over

=item !flatten()

Return self, as Arrays are already flattened.

=cut

.namespace ['Perl6Array']
.sub '!flatten' :method
    .return (self)
.end

=item !STORE()

Store things into an Array (e.g., upon assignment)

=cut

.namespace ['Perl6Array']
.sub '!STORE' :method
    .param pmc source
    .local pmc array, it, type
    type = self.'of'()
    ## we create a new array here instead of emptying self in case
    ## the source argument contains self or elements of self.
    array = root_new ['parrot';'ResizablePMCArray']
    source = 'list'(source)
    it = iter source
  array_loop:
    unless it goto array_done
    $P0 = shift it
    $I0 = type.'ACCEPTS'($P0)
    unless $I0 goto type_error
    $P0 = '!CALLMETHOD'('Scalar',$P0)
    $P1 = clone $P0
    .fixup_cloned_sub($P0, $P1)
    setprop $P1, 'type', type
    push array, $P1
    goto array_loop
  array_done:
    $I0 = elements self
    splice self, array, 0, $I0
    .return (self)
  type_error:
    $S0 = '!make_type_fail_message'('Array assignment', $P0, type)
    'die'($S0)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
