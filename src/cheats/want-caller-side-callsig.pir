=item !deconstruct_call_sig

Transforms a capture into positional and named parts.

XXX Eventually we will have caller-side :call_sig and won't have to do this.

=cut

.namespace []
.sub '!deconstruct_call_sig'
    .param pmc call_sig
    .local pmc pos_args, named_args, names
    
    pos_args = new ['ResizablePMCArray']
    $I0 = elements call_sig
    $I1 = 0
  pos_loop:
    if $I1 == $I0 goto pos_loop_end
    $P0 = call_sig[$I1]
    pos_args[$I1] = $P0
    inc $I1
    goto pos_loop
  pos_loop_end:

    named_args = new ['Hash']
    names = getattribute call_sig, 'named'
    if null names goto named_loop_end
    $I0 = elements names
    $I1 = 0
  named_loop:
    if $I1 == $I0 goto named_loop_end
    $S0 = names[$I1]
    $P0 = call_sig[$S0]
    named_args[$S0] = $P0
    inc $I1
    goto named_loop
  named_loop_end:
    
    .return (pos_args, named_args)
.end
