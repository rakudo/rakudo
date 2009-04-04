## $Id$

=head1 NAME

src/classes/Positional.pir - Positional Role

=head1 DESCRIPTION

=cut

.namespace ['Positional[::T]']

.sub '_positional_role_body'
    .param pmc type :optional
    
    # Capture type.
    if null type goto no_type
    type = type.'WHAT'()
    goto type_done
  no_type:
    type = get_hll_global 'Object'
  type_done:
    .lex 'T', type
    
    # Create role.
    .local pmc metarole
    metarole = "!meta_create"("role", "Positional[::T]", 0)
    .create_parametric_role(metarole)
.end
.sub '' :load :init
    .local pmc block, signature
    block = get_hll_global ['Positional[::T]'], '_positional_role_body'
    signature = new ["Signature"]
    setprop block, "$!signature", signature
    signature."!add_param"("T", 1 :named("optional"))
    "!ADDTOROLE"(block)
.end


=head2 Operators

=over

=item postcircumfix:<[ ]>

Returns a list element or slice.

=cut

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
    $P0 = args[0]
    $I0 = isa $P0, ['Whatever']
    if $I0 goto result_whatever
    $I0 = args[0]
    if $I0 >= 0 goto result_fetch
    result = 'undef'()
    goto end
  result_fetch:
    result = self[$I0]
    unless null result goto end
    result = 'undef'()
    self[$I0] = result
    goto end
  result_whatever:
    result = 'list'(self)
    goto end
  slice:
    result = new ['List']
  slice_loop:
    unless args goto slice_done
    $P0 = shift args
    $I0 = isa $P0, ['Whatever']
    if $I0 goto slice_whatever
    $I0 = $P0
    if $I0 >= 0 goto slice_index
  slice_negative:
    .local pmc elem
    elem = 'undef'()
    goto slice_elem
  slice_index:
    elem = self[$I0]
    unless null elem goto slice_elem
    elem = 'undef'()
    self[$I0] = elem
  slice_elem:
    push result, elem
    goto slice_loop
  slice_whatever:
    ##  add all of the elements to the result
    $I0 = elements result
    splice result, self, $I0, 0
    goto slice_loop
  slice_done:
  end:
    .return (result)
.end
.sub '' :load :init
    .local pmc block, signature
    block = get_hll_global ['Positional[::T]'], 'postcircumfix:[ ]'
    signature = new ["Signature"]
    setprop block, "$!signature", signature
    signature."!add_param"("@args", 0 :named("named"))
    signature."!add_param"("%options", 1 :named("named"))
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
    $P0 = get_hll_global ['Positional[::T]'], 'postcircumfix:[ ]'
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
    .tailcall 'postcircumfix:[ ]'(invocant, args, options :flat :named)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:

