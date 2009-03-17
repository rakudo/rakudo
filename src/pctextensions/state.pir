# Copyright (C) 2007-2009, The Perl Foundation.

=head1 NAME

state.pir - supports the state scope type

=head1 DESCRIPTION

This is a kind of "plug-in" to PAST::Compiler that adds the state scope type.
This may or may not get folded back into PCT some day.

XXX TODO: Doesn't yet handle binding beyond the initial one.

=cut

.include "interpinfo.pasm"
.namespace [ 'PAST';'Compiler' ]
.sub 'state' :method :multi(_, ['PAST';'Var'])
    .param pmc node
    .param pmc bindpost

    .local string name
    $P0 = get_hll_global ['POST'], 'Ops'
    name = node.'name'()
    name = self.'escape'(name)

    .local int isdecl
    isdecl = node.'isdecl'()

    if bindpost goto lexical_bind

  lexical_post:
    if isdecl goto lexical_decl
    .local pmc ops, fetchop, storeop
    ops = $P0.'new'('node'=>node)
    $P0 = get_hll_global ['POST'], 'Op'
    fetchop = $P0.'new'(ops, name, 'pirop'=>'find_lex')
    storeop = $P0.'new'(name, ops, 'pirop'=>'store_lex')
    .tailcall self.'vivify'(node, ops, fetchop, storeop)

  lexical_decl:
    ops = $P0.'new'('node'=>node)
    
    # Do a call to restore any previous values. We can skip the rest
    # if it returns a false value.
    $P0 = self.'uniquereg'('I')
    ops.'push_pirop'('call', '"!state_var_init"', name, 'result'=>$P0)
    $P1 = get_hll_global ['POST'], 'Label'
    $S0 = self.'unique'('state')
    $P1 = $P1.'new'('result'=>$S0)
    ops.'push_pirop'('unless', $P0, $P1)
    
    # Vivify and store vivification.
    .local pmc viviself, vivipost
    viviself = node.'viviself'()
    vivipost = self.'as_vivipost'(viviself, 'rtype'=>'P')
    ops.'push'(vivipost)
    ops.'push_pirop'('.lex', name, vivipost)
    $P0 = self.'uniquereg'('P')
    ops.'push_pirop'('interpinfo', $P0, .INTERPINFO_CURRENT_SUB)
    ops.'push_pirop'('getprop', $P0, '"$!state_store"', $P0)
    $S0 = $P0
    concat $S0, "["
    concat $S0, name
    concat $S0, "]"
    ops.'push_pirop'('set', $S0, vivipost)
    ops.'result'(vivipost)

    # Finally, label we go to if we don't need to init.
    ops.'push'($P1)
    .return (ops)

  lexical_bind:
    $P0 = get_hll_global ['POST'], 'Op'
    if isdecl goto lexical_bind_decl
    .tailcall $P0.'new'(name, bindpost, 'pirop'=>'store_lex', 'result'=>bindpost)
  lexical_bind_decl:
    .tailcall $P0.'new'(name, bindpost, 'pirop'=>'.lex', 'result'=>bindpost)
.end
