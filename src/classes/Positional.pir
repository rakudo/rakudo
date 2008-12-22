## $Id$

=head1 NAME

src/classes/Positional.pir - Positional Role

=head1 DESCRIPTION

=cut

.namespace []

.sub '' :anon :load :init
    .local pmc positional
    positional = '!keyword_role'('Positional')
.end

=head2 Operators

=over

=item postcircumfix:<[ ]>

Returns a list element or slice.

=cut

.namespace ['Positional']
.sub 'postcircumfix:[ ]' :method
    .param pmc args            :slurpy
    .param pmc options         :slurpy :named
    .local pmc result
    if args goto do_index
    ## return complete invocant as a list
    .tailcall self.'list'()
  do_index:
    args.'!flatten'()
    $I0 = args.'elems'()
    if $I0 != 1 goto slice
    $I0 = args[0]
    if $I0 >= 0 goto result_fetch
    result = new 'Failure'
    goto end
  result_fetch:
    result = self[$I0]
    unless null result goto end
    result = new 'Failure'
    self[$I0] = result
    goto end
  slice:
    result = new 'List'
  slice_loop:
    unless args goto slice_done
    $I0 = shift args
    if $I0 >= 0 goto slice_fetch
    .local pmc elem
    elem = new 'Failure'
    goto slice_elem
  slice_fetch:
    elem = self[$I0]
    unless null elem goto slice_elem
    elem = new 'Failure'
    self[$I0] = elem
  slice_elem:
    push result, elem
    goto slice_loop
  slice_done:
  end:
    .return (result)
.end

.namespace []
.sub 'postcircumfix:[ ]' :multi(_)
    .param pmc invocant
    .param pmc args    :slurpy
    .param pmc options :slurpy :named
    $I0 = can invocant, 'postcircumfix:[ ]'
    if $I0 goto object_method
    $I0 = isa invocant, 'Perl6Object'
    if $I0 goto object_method
  foreign:
    $P0 = get_hll_global ['Positional'], 'postcircumfix:[ ]'
    .tailcall $P0(invocant, args :flat, options :flat :named)
  object_method:
    .tailcall invocant.'postcircumfix:[ ]'(args :flat, options :flat :named)
.end


.sub 'postcircumfix:[ ]' :multi(_, 'Sub')
    .param pmc invocant
    .param pmc argsblock
    .param pmc options :slurpy :named
    $I0 = elements invocant
    $P0 = box $I0
    set_hll_global ['Whatever'], '$!slice', $P0
    .local pmc args
    args = argsblock()
    args = 'list'(args)
    .tailcall 'postcircumfix:[ ]'(invocant, args :flat, options :flat :named)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:

