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
    $P1 = $P1.'new'($P0, 0, '')
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


=item !dispatch_.^

Does a call on the metaclass.

=cut

.sub '!dispatch_.^'
    .param pmc invocant
    .param string name
    .param pmc pos_args   :slurpy
    .param pmc named_args :slurpy :named
    .local pmc how
    invocant = descalarref invocant
    how = invocant.'HOW'()
    .tailcall how.name(invocant, pos_args :flat, named_args :flat :named)
.end


=item !dispatch_.=

Does a call on the LHS and assigns the result back to it.

=cut

.sub '!dispatch_.='
    .param pmc invocant
    .param string name
    .param pmc pos_args   :slurpy
    .param pmc named_args :slurpy :named
    .param pmc result
    result = invocant.name(pos_args :flat, named_args :flat :named)
    .tailcall '&infix:<=>'(invocant, result)
.end


=item !dispatch_.?

Implements the .? operator. Calls at most one matching method, and returns
a failure if there is none.

=cut

.sub '!dispatch_.?'
    .param pmc invocant
    .param string method_name
    .param pmc pos_args   :slurpy
    .param pmc named_args :slurpy :named

    # Use .can to try and get us a method to call.
    $P0 = invocant.'HOW'()
    $P0 = $P0.'can'(invocant, method_name)
    unless $P0 goto error
    push_eh check_error
    $P1 = $P0(invocant, pos_args :flat, named_args :named :flat)
    .return ($P1)
  check_error:
    .local pmc exception
    .get_results (exception)
    pop_eh
    if exception == "No candidates found to invoke" goto error
    rethrow exception

  error:
    .tailcall '!FAIL'('Undefined value returned by invocation of undefined method')
.end


=item !dispatch_.*

Implements the .* operator. Calls one or more matching methods.

=cut

.sub '!dispatch_.*'
    .param pmc call_sig :call_sig

    # Deconstruct call signature (no caller side :call_sig yet).
    .local pmc invocant, pos_args, named_args
    .local string method_name
    invocant = shift call_sig
    method_name = shift call_sig
    (pos_args, named_args) = '!deconstruct_call_sig'(call_sig)
    unshift call_sig, invocant

    # Set up result list.
    .local pmc result_list
    result_list = new ['ResizablePMCArray']

    # Get all possible methods.
    .local pmc methods
    $P0 = invocant.'HOW'()
    methods = $P0.'can'(invocant, method_name)
    unless methods goto it_loop_end

    # Call each method, expanding out any multis along the way.
    .local pmc res_parcel, it, multi_it, cur_meth
    it = iter methods
  it_loop:
    unless it goto it_loop_end
    cur_meth = shift it
    $P0 = cur_meth
    $I0 = isa $P0, 'P6Invocation'
    unless $I0 goto did_deref
    $P0 = deref cur_meth
  did_deref:
    $I0 = isa $P0, 'Perl6MultiSub'
    if $I0 goto is_multi
    push_eh check_error
    res_parcel = cur_meth(invocant, pos_args :flat, named_args :named :flat)
    pop_eh
    res_parcel = root_new ['parrot';'ObjectRef'], res_parcel
    push result_list, res_parcel
    goto it_loop
  is_multi:
    $P0 = $P0.'find_possible_candidates'(call_sig)
    multi_it = iter $P0
  multi_it_loop:
    unless multi_it goto it_loop
    cur_meth = shift multi_it
    res_parcel = cur_meth(invocant, pos_args :flat, named_args :named :flat)
    push result_list, res_parcel
    goto multi_it_loop
  check_error:
    .local pmc exception
    .get_results (exception)
    pop_eh
    if exception == "No candidates found to invoke" goto it_loop
    rethrow exception
  it_loop_end:

    result_list = '&infix:<,>'(result_list :flat)
    .tailcall '&list'(result_list)
.end


=item !.+

Implements the .+ operator. Calls one or more matching methods, dies if
there are none.

=cut

.sub '!dispatch_.+'
    .param pmc invocant
    .param string method_name
    .param pmc pos_args     :slurpy
    .param pmc named_args   :slurpy :named

    # Use .* to produce a (possibly empty) list of result captures.
    .local pmc result_list
    result_list = '!dispatch_.*'(invocant, method_name, pos_args :flat, named_args :flat :named)

    # If we got no elements at this point, we must die.
    $I0 = result_list.'elems'()
    if $I0 == 0 goto failure
    .return (result_list)
  failure:
    $S0 = "Could not invoke method '"
    concat $S0, method_name
    concat $S0, "' on invocant of type '"
    $S1 = invocant.'WHAT'()
    concat $S0, $S1
    concat $S0, "'"
    '&die'($S0)
.end


=item !deferal_fail

Used by P6invocation to help us get soft-failure semantics when no deferal
is possible.

=cut

.sub '!deferal_fail'
    .param pmc pos_args    :slurpy
    .param pmc named_args  :slurpy :named
    .lex '__CANDIDATE_LIST__', $P0
    .tailcall '!FAIL'('No method to defer to')
.end


=item !postcircumfix_forwarder

When we call a postcircumfix:<( )> we need to make sure we pass along just
one positional that is a capture.

=cut

.sub '!postcircumfix_forwarder'
    .param pmc method
    .param pmc invocant
    .param pmc pos   :slurpy
    .param pmc named :slurpy :named
    .local pmc cappy
    cappy = get_hll_global 'Capture'
    cappy = cappy.'new'(pos :flat, named :flat :named)
    method(invocant, cappy)
.end
