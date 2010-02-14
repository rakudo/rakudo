=head1 TITLE

src/cheats/any-map.pir - lazy mapping

=head1 DESCRIPTION

This code handles the 'map' function on Any.
We use a private class as a generator for keeping
track of the iterator and block, and handling
any redo/next/last exceptions that might occur
during the block.

The result of each block execution is returned
as the result of the overall Map.

=cut

.namespace ['!Mapper']
.sub '' :anon :init :load
    $P0 = newclass ['!Mapper']
    addattribute $P0, '$!list_it'
    addattribute $P0, '&!block'
.end

.sub '!generate' :method
    .param int n               :optional
    .param int has_n           :opt_flag

    .local pmc retlist
    retlist = root_new ['parrot';'ResizablePMCArray']

    unless has_n goto n_done
    if n < 1 goto iter_max
  n_done:

    .local pmc list_it, block
    list_it = getattribute self, '$!list_it'
    block   = getattribute self, '&!block'
    .local int count
    count = block.'count'()

  iter_loop:
    unless list_it goto iter_done

    .local pmc arglist
    arglist = root_new ['parrot';'ResizablePMCArray']
    .local int c
    c = count
  arglist_loop:
    unless list_it goto arglist_done
    unless c > 0 goto arglist_done
    $P0 = shift list_it
    push arglist, $P0
    dec c
    goto arglist_loop
  arglist_done:
    .local pmc result
    $P0 = block(arglist :flat)
    push retlist, $P0
    unless has_n goto iter_loop
    $I0 = elements retlist
    if $I0 < n goto iter_loop

  iter_max:
    push retlist, self
  iter_done:
    .return (retlist)
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:

