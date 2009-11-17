=item !MAKE_WHATEVER_CLOSURE

Creates whatever closures (*.foo => { $_.foo })

=cut

.namespace []
.sub '!MAKE_WHATEVER_CLOSURE'
    .param pmc whatever
    .param pmc pos_args   :slurpy
    .param pmc named_args :slurpy :named
    .local pmc name
    $P0 = getinterp
    $P0 = $P0['sub']
    name = getprop 'name', $P0
    .lex '$name', name
    .lex '$pos_args', pos_args
    .lex '$named_args', named_args
    .const 'Sub' $P0 = '!whatever_dispatch_helper'
    $P0 = newclosure $P0
    $P1 = get_hll_global 'Block'
    $P1 = $P1.'new'($P0, 0)
    .return ($P1)
.end
.sub '!whatever_dispatch_helper' :outer('!MAKE_WHATEVER_CLOSURE')
    .param pmc obj
    $P0 = find_lex '$name'
    $S0 = $P0
    $P1 = find_lex '$pos_args'
    $P2 = find_lex '$named_args'
    .tailcall obj.$S0($P1 :flat, $P2 :flat :named)
.end


=item !dispatch_dispatcher_parallel

Does a parallel method dispatch over an existing dispatcher. Just invokes the normal
dispatcher for each thingy we're dispatching over.

=cut

.sub '!dispatch_dispatcher_parallel'
    .param pmc invocanty
    .param string dispatcher
    .param pmc pos_args        :slurpy
    .param pmc named_args      :slurpy :named

    .local pmc it, results, disp
    disp = find_name dispatcher
    results = new ['ResizablePMCArray']
    invocanty = invocanty.'list'()
    it = iter invocanty
  it_loop:
    unless it goto it_loop_done
    $P0 = shift it
    $P0 = disp($P0, pos_args :flat, named_args :flat :named)
    push results, $P0
    goto it_loop
  it_loop_done:

    .tailcall '&infix:<,>'(results :flat)
.end


=item !dispatch_method_parallel

Does a parallel method dispatch. Invokes the method for each thing in the
array of invocants.

=cut

.sub '!dispatch_method_parallel'
    .param pmc invocanty
    .param string name
    .param pmc pos_args        :slurpy
    .param pmc named_args      :slurpy :named

    .local pmc it, results
    results = new ['ResizablePMCArray']
    invocanty = invocanty.'list'()
    it = iter invocanty
  it_loop:
    unless it goto it_loop_done
    $P0 = shift it
    $P0 = $P0.name(pos_args :flat, named_args :flat :named)
    push results, $P0
    goto it_loop
  it_loop_done:

    .tailcall '&infix:<,>'(results :flat)
.end
