## $Id$

=head1 NAME

src/classes/Callable.pir - Callable Role

=head1 DESCRIPTION

This implements the parametric role Callable[::T = Object].

=cut

.namespace ['Callable[T]']

.sub '_callable_role_body'
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
    metarole = "!meta_create"("role", "Callable[T]", 0)
    .create_parametric_role(metarole)
.end
.sub '' :load :init :outer('_callable_role_body')
    .local pmc block, signature
    block = get_hll_global ['Callable[T]'], '_callable_role_body'
    signature = new ["Signature"]
    setprop block, "$!signature", signature
    signature."!add_param"("T", 1 :named("optional"))
    "!ADDTOROLE"(block)
.end

.sub 'returns' :method :outer('_callable_role_body')
    $P0 = find_lex 'T'
    .return ($P0)
.end
.sub '' :load :init :outer('_callable_role_body')
    .local pmc block, signature
    block = get_hll_global ['Callable[T]'], 'returns'
    signature = new ["Signature"]
    setprop block, "$!signature", signature
.end

.sub 'of' :method :outer('_callable_role_body')
    $P0 = find_lex 'T'
    .return ($P0)
.end
.sub '' :load :init :outer('_callable_role_body')
    .local pmc block, signature
    block = get_hll_global ['Callable[T]'], 'of'
    signature = new ["Signature"]
    setprop block, "$!signature", signature
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:

