## $Id$

=head1 NAME

src/classes/Callable.pir - Callable Role

=head1 DESCRIPTION

This implements the parametric role Callable[::T = Object].

=cut

.namespace ['Callable[::T]']

.sub '' :load :init
    # Create a parametric role with 1 possible candidate.
    .local pmc role
    .const 'Sub' $P0 = '_callable_role_body'
    role = new ['Perl6Role']
    $P1 = box 'Callable'
    setattribute role, '$!shortname', $P1
    role.'!add_variant'($P0)
    set_hll_global 'Callable', role
.end


# This defines the body of the role, which is run per type the role is
# parameterized with.
.sub '_callable_role_body'
    .param pmc type :optional

    $P0 = get_hll_global ['Callable[::T]'], 'of'
    capture_lex $P0
    $P0 = get_hll_global ['Callable[::T]'], 'returns'
    capture_lex $P0

    # Capture type.
    if null type goto no_type
    type = type.'WHAT'()
    goto type_done
  no_type:
    type = get_hll_global 'Object'
  type_done:
    .lex 'T', type

    # Create role.
    .const 'Sub' $P0 = 'callable_of'
    capture_lex $P0
    .tailcall '!create_parametric_role'("Callable[::T]")
.end
.sub '' :load :init
    .local pmc block, signature
    block = get_hll_global ['Callable[::T]'], '_callable_role_body'
    signature = allocate_signature 1
    setprop block, "$!signature", signature
    null $P1
    set_signature_elem signature, 0, "T", SIG_ELEM_IS_OPTIONAL, $P1, $P1, $P1, $P1, $P1, $P1
.end


=item returns

Returns the type constraining what may be returned.

=cut

.sub 'returns' :method :outer('_callable_role_body')
    $P0 = find_lex 'T'
    .return ($P0)
.end
.sub '' :load :init
    .local pmc block, signature
    block = get_hll_global ['Callable[::T]'], 'returns'
    signature = allocate_signature 0
    setprop block, "$!signature", signature
.end


=item of

Returns the type constraining what may be returned.

=cut

.sub 'of' :method :outer('_callable_role_body') :subid('callable_of')
    $P0 = find_lex 'T'
    .return ($P0)
.end
.sub '' :load :init
    .local pmc block, signature
    block = get_hll_global ['Callable[::T]'], 'of'
    signature = allocate_signature 0
    setprop block, "$!signature", signature
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:

