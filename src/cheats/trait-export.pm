our multi trait_mod:<is>(Code $block, $arg?, :$export!) {
    # Maybe we can re-write some more of this out of PIR and into Perl 6.
    Q:PIR {
    .local pmc block, arg
    block = find_lex '$block'
    arg = find_lex '$arg'

    # Get the name from the underlying Parrot Sub.
    .local string blockname
    blockname = block.'do'()

    # Need & added to the name.
    blockname = concat '&', blockname

    # Create the EXPORT namespace if it doesn't already exist.
    # Assume our caller is coming from the namespace where exporting
    # is to take place.
    .local pmc blockns, exportns
    $P0 = getinterp
    blockns = $P0['namespace';1]
    exportns = blockns.'make_namespace'('EXPORT')

    # Multisubs/multimethods export as Perl6MultiSub -- keep a flag
    # that we can use to test this later.
    .local int ismulti
    $P0 = block.'multi'()
    ismulti = istrue $P0

    .local pmc tags, tags_it
    tags = split ' ', 'ALL DEFAULT'
    tags_it = iter tags
  tags_loop:
    unless tags_it goto tags_done
    .local string tagname
    .local pmc tagns
    tagname = shift tags_it
    tagns = exportns.'make_namespace'(tagname)
    if ismulti goto export_multi
    # Non-multi, so just export the block directly.
    tagns[blockname] = block
    goto tags_loop
  export_multi:
    # MultiSub candidate, so vivify a Perl6MultiSub in the export
    # namespace and push this candidate onto it.
    $P0 = vivify tagns, blockname, ['Perl6MultiSub']
    push $P0, block
    goto tags_loop
  tags_done:
    }
}
