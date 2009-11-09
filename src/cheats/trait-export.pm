our multi trait_mod:<is>(Code $block, $arg?, :$export!) {
    # Maybe we can re-write some more of this out of PIR and into Perl 6.
    Q:PIR {
    .local pmc block, arg
    block = find_lex '$block'
    arg = find_lex '$arg'

    # Multis that are exported need to be whole-sale exported as multis.
    .local pmc blockns
    .local string blockname
    block = block.'do'()
    blockns = block.'get_namespace'()
    blockname = block
    $P0 = blockns[blockname]
    $I0 = isa $P0, 'MultiSub'
    unless $I0 goto multi_handled
    block = $P0
  multi_handled:

    .local pmc exportns
    exportns = blockns.'make_namespace'('EXPORT')
    unless arg goto default_export
    .local pmc it
    $I0 = arg.'elems'()
    if $I0 goto have_arg
  default_export:
    $P0 = get_hll_global 'Pair'
    $P0 = $P0.'new'('key' => 'DEFAULT', 'value' => 1)
    arg = '&infix:<,>'($P0)
  have_arg:
    it = iter arg
  arg_loop:
    unless it goto arg_done
    .local pmc tag, ns
    tag = shift it
    $I0 = isa tag, 'Pair'
    unless $I0 goto arg_loop
    $S0 = tag.'key'()
    ns = exportns.'make_namespace'($S0)
    ns[blockname] = block
    goto arg_loop
  arg_done:
    ns = exportns.'make_namespace'('ALL')
    ns[blockname] = block
    }
}
