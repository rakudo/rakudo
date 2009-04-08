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
    '!EXPORT'('delete,exists,pop,push,shift,unshift', 'from'=>$P0, 'to_p6_multi'=>1)
.end

=head2 Methods

=over

=item delete

Remove items from an array.

=cut

.namespace ['Perl6Array']
.sub 'delete' :method :multi() :subid('array_delete')
    .param pmc indices :slurpy
    .local pmc result
    result = new 'List'
    null $P99

    indices.'!flatten'()
  indices_loop:
    unless indices goto indices_end
    $I0 = shift indices
    $P0 = self[$I0]
    push result, $P0
    self[$I0] = $P99

  shorten:
    $I0 = self.'elems'()
    dec $I0
  shorten_loop:
    if $I0 < 0 goto shorten_end
    $P0 = self[$I0]
    if null $P0 goto do_shorten
    $I1 = $P0.'defined'()
    if $I1 goto shorten_end
  do_shorten:
    delete self[$I0]
    dec $I0
    goto shorten_loop
  shorten_end:
    goto indices_loop

  indices_end:
    .return (result)
.end
.sub '' :init :load
    .local pmc block, signature
    .const 'Sub' $P0 = "array_delete"
    block = $P0
    signature = new ["Signature"]
    setprop block, "$!signature", signature
    signature."!add_param"("@indices", 1 :named("slurpy"))
    $P0 = get_hll_global 'Array'
    signature."!add_implicit_self"($P0)
.end


=item exists(indices :slurpy)

Return true if the elements at C<indices> have been assigned to.

=cut

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
    signature = new ["Signature"]
    setprop block, "$!signature", signature
    signature."!add_param"("@indices", 1 :named("slurpy"))
    $P0 = get_hll_global 'Array'
    signature."!add_implicit_self"($P0)
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
    signature = new ["Signature"]
    setprop block, "$!signature", signature
    $P0 = get_hll_global 'Array'
    signature."!add_implicit_self"($P0)
.end


=item push(args :slurpy)

Add C<args> to the end of the Array.

=cut

.sub 'push' :method :multi() :subid('array_push')
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
    $I0 = elements self
    splice self, args, $I0, 0
    .tailcall self.'elems'()
  type_error:
    'die'('Type check failure in push')
.end
.sub '' :init :load
    .local pmc block, signature
    .const 'Sub' $P0 = "array_push"
    block = $P0
    signature = new ["Signature"]
    setprop block, "$!signature", signature
    signature."!add_param"("@items", 1 :named("slurpy"))
    $P0 = get_hll_global 'Array'
    signature."!add_implicit_self"($P0)
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
    signature = new ["Signature"]
    setprop block, "$!signature", signature
    $P0 = get_hll_global 'Array'
    signature."!add_implicit_self"($P0)
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
    .tailcall self.'elems'()
  type_error:
    'die'('Type check failure in push')
.end
.sub '' :init :load
    .local pmc block, signature
    .const 'Sub' $P0 = "array_unshift"
    block = $P0
    signature = new ["Signature"]
    setprop block, "$!signature", signature
    signature."!add_param"("@items", 1 :named("slurpy"))
    $P0 = get_hll_global 'Array'
    signature."!add_implicit_self"($P0)
.end

=item values()

Return Array as a List of its values.

=cut

.namespace ['Perl6Array']
.sub 'values' :method
    $P0 = new 'List'
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
    array = new 'ResizablePMCArray'
    source = 'list'(source)
    it = iter source
  array_loop:
    unless it goto array_done
    $P0 = shift it
    $I0 = type.'ACCEPTS'($P0)
    unless $I0 goto type_error
    $P0 = '!CALLMETHOD'('Scalar',$P0)
    $P0 = clone $P0
    setprop $P0, 'type', type
    push array, $P0
    goto array_loop
  array_done:
    $I0 = elements self
    splice self, array, 0, $I0
    .return (self)
  type_error:
    'die'("Type mismatch in assignment to Array.")
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
