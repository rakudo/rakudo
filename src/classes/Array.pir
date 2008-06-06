## $Id$

=head1 NAME

src/classes/Array.pir - Perl 6 Array class and related functions

=head2 Object Methods

=cut

.sub 'onload' :anon :load :init
    .local pmc p6meta, arrayproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    arrayproto = p6meta.'new_class'('Perl6Array', 'parent'=>'List', 'name'=>'Array')
    p6meta.'new_class'('Arrayref', 'parent'=>arrayproto, 'protoobject'=>arrayproto)

    $P0 = get_hll_namespace ['Perl6Array']
    '!EXPORT'('delete exists pop push shift unshift', 'from'=>$P0)
.end


.namespace ['Perl6Array']
.sub 'infix:=' :method
    .param pmc source
    $P0 = source.'list'()
    $P0 = clone $P0
    $I0 = elements self
    splice self, $P0, 0, $I0
    .return (self)
.end


=head2 Array methods

=over 4

=item delete(indices :slurpy)

Delete the elements specified by C<indices> from the array
(i.e., replace them with null).  We also shorten the array
to the length of the last non-null (existing) element.

=cut

.namespace ['Perl6Array']

.sub 'delete' :method :multi(Perl6Array)
    .param pmc indices :slurpy
    .local pmc result
    result = new 'List'
    null $P99

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


=item pop()

Remove the last item from the array and return it.

=cut

.sub 'pop' :method :multi(Perl6Array)
    .local pmc x
    unless self goto empty
    x = pop self
    goto done
  empty:
    x = new 'Failure'
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
    x = new 'Failure'
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


=back

=head1  Arrayref

=cut

.namespace []

.sub '!Arrayref'
    .param pmc args            :slurpy
    .local pmc result, iter
    args = 'list'(args)
    result = new 'Arrayref'
    iter = args.'iterator'()
  iter_loop:
    unless iter goto iter_end
    $P0 = shift iter
    $P0 = clone $P0
    push result, $P0
    goto iter_loop
  iter_end:
    .return (result)
.end


.namespace ['Arrayref']

.sub 'item' :method
    .return (self)
.end


.sub 'list' :method
    $P0 = new 'List'
    push $P0, self
    .return ($P0)
.end


# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
