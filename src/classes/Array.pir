## $Id$

=head1 NAME

src/classes/Array.pir - Perl 6 Array class and related functions

=head2 Object Methods

=cut

.sub 'onload' :anon :load :init
    .local pmc p6meta, arrayproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    arrayproto = p6meta.'new_class'('Perl6Array', 'parent'=>'List', 'name'=>'Array')

    $P0 = get_hll_namespace ['Perl6Array']
    '!EXPORT'('delete exists pop push shift unshift', 'from'=>$P0)
.end


.namespace []
.sub 'circumfix:[ ]'
    .param pmc values          :slurpy
    $P0 = new 'Perl6Array'
    $I0 = elements values
    splice $P0, values, 0, $I0
    $P0.'!flatten'()
    $P1 = new 'ObjectRef', $P0
    .return ($P1)
.end


=head2 Array methods

=over 4

=item Scalar()

Returns an ObjectRef referencing itself, unless it already is one in which
case just returns as is.

=cut

.namespace ['Perl6Array']

.sub 'Scalar' :method
    $I0 = isa self, 'ObjectRef'
    unless $I0 goto not_ref
    .return (self)
  not_ref:
    $P0 = new 'ObjectRef', self
    .return ($P0)
.end


.sub 'delete' :method :multi(Perl6Array)
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
    unless null $P0 goto shorten_end
    delete self[$I0]
    dec $I0
    goto shorten_loop
  shorten_end:
    goto indices_loop

  indices_end:
    .return (result)
.end


=item exists(indices :slurpy)

Return true if the elements at C<indices> have been assigned to.

=cut

.sub 'exists' :method :multi(Perl6Array)
    .param pmc indices :slurpy
    .local int test

    test = 0
  indices_loop:
    unless indices goto indices_end
    $I0 = shift indices
    test = exists self[$I0]
    if test goto indices_loop
  indices_end:
    .return 'prefix:?'(test)
.end


=item item()

Return Array in item context (i.e., self)

=cut

.sub 'item' :method
    .return (self)
.end


=item pop()

Remove the last item from the array and return it.

=cut

.sub 'pop' :method :multi(Perl6Array)
    .local pmc x
    unless self goto empty
    x = pop self
    goto done
  empty:
    x = '!FAIL'('Undefined value popped from empty array')
  done:
    .return (x)
.end


=item push(args :slurpy)

Add C<args> to the end of the Array.

=cut

.sub 'push' :method :multi(Perl6Array)
    .param pmc args :slurpy
    args.'!flatten'()
    $I0 = elements self
    splice self, args, $I0, 0
    .return self.'elems'()
.end


=item shift()

Shift the first item off the array and return it.

=cut

.sub 'shift' :method :multi(Perl6Array)
    .local pmc x
    unless self goto empty
    x = shift self
    goto done
  empty:
    x = '!FAIL'('Undefined value shifted from empty array')
  done:
    .return (x)
.end


=item unshift(args :slurpy)

Adds C<args> to the beginning of the Array.

=cut

.sub 'unshift' :method :multi(Perl6Array)
    .param pmc args :slurpy
    args.'!flatten'()
    splice self, args, 0, 0
    .return self.'elems'()
.end


=item values()

Return the values of the Array as a List.

=cut

.sub 'values' :method
    $P0 = new 'List'
    splice $P0, self, 0, 0
    .return ($P0)
.end


# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
