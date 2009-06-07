## $Id$

=head1 NAME

src/classes/Positional.pir - Positional Role

=head1 DESCRIPTION

=cut

.namespace ['Positional[::T]']

.sub '_positional_role_body'
    .param pmc type :optional

    .const 'Sub' $P0 = 'Positional::postcircumfix:[Int]'
    capture_lex $P0
    .const 'Sub' $P1 = 'Positional::of'
    capture_lex $P1
    
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
    .tailcall '!create_parametric_role'(metarole)
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

=cut

.sub 'postcircumfix:[ ]' :method :multi(_, 'Integer') :outer('_positional_role_body') :subid('Positional::postcircumfix:[Int]')
    .param int index
    .param pmc options         :slurpy :named
    .local pmc result
    if index < 0 goto err_undef
    .local pmc type
    type = find_lex 'T'
    .local int count
    count = elements self
  extend_loop:
    unless count < index goto extend_done
    result = 'undef'()
    setprop result, 'type', type
    self[count] = result
    inc count
    goto extend_loop
  extend_done:
    result = self[index]
    unless null result goto done
    result = 'undef'()
    setprop result, 'type', type
    self[index] = result
  done:
    .return (result)
  err_undef:
    result = 'undef'()
    .return (result)
.end

.sub 'postcircumfix:[ ]' :method :multi(_, 'Sub')
    .param pmc arg
    .param pmc options         :slurpy :named
    $I0 = elements self
    $P0 = arg($I0)
    .tailcall 'postcircumfix:[ ]'(self, $P0, options :named :flat)
.end

.sub 'postcircumfix:[ ]' :method :multi(_, 'Whatever')
    .param pmc arg
    .param pmc options         :slurpy :named
    .tailcall 'list'(self)
.end

.sub 'postcircumfix:[ ]' :method :multi(_)
    .param pmc options         :slurpy :named
    .tailcall self.'list'()
.end

.sub 'postcircumfix:[ ]' :method :multi(_, _)
    .param pmc args            :slurpy
    .param pmc options         :slurpy :named
    .local pmc result
    args = 'list'(args)
    $I0 = elements args
    if $I0 == 1 goto arg_slice
    result = new ['List']
  args_loop:
    unless args goto args_done
    $P0 = shift args
    $P0 = 'postcircumfix:[ ]'(self, $P0, options :named :flat)
    $P0 = 'list'($P0)
    $I0 = elements result
    splice result, $P0, $I0, 0
    goto args_loop
  args_done:
    .return (result)
  arg_slice:
    $P0 = args[0]
    .const 'Sub' $P1 = 'Positional::postcircumfix:[Int]'
    .tailcall $P1(self, $P0, options :named :flat)
.end
    
.sub '' :load :init
    .local pmc block, signature
    .const 'Sub' block1 = 'Positional::postcircumfix:[Int]'
    signature = new ["Signature"]
    setprop block1, "$!signature", signature
#    signature."!add_param"("$args", 0 :named("named"))
#    signature."!add_param"("$options", 1 :named("named"))
.end


=item of

Returns the type constraining what may be stored.

=cut

.sub 'of' :method :outer('_positional_role_body') :subid('Positional::of')
    $P0 = find_lex 'T'
    .return ($P0)
.end
.sub '' :load :init
    .local pmc block, signature
    .const 'Sub' block = 'Positional::of'
    signature = new ["Signature"]
    setprop block, "$!signature", signature
.end


.namespace []
.sub 'postcircumfix:[ ]'
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

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:

