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
