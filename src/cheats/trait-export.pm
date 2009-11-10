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

    .local pmc exportns, tagns
    exportns = blockns.'make_namespace'('EXPORT')
    tagns = exportns.'make_namespace'('DEFAULT')
    tagns[blockname] = block
    tagns = exportns.'make_namespace'('ALL')
    tagns[blockname] = block
    }
}
