## $Id$

=head1 NAME

src/parrot/P6Invocation - extra methods for the P6Invocation PMC

=head2 Methods on P6Invocation

We also add some methods to P6Invocation.

=item !flatten

Here so that list(...) will behave nicely. No doubt can change substantially
when we have laziness support.

=cut

.namespace ["P6Invocation"]
.sub '!flatten' :method
    .local pmc result
    result = new ['ResizablePMCArray']
  it_loop:
    unless self goto it_loop_end
    $P0 = shift self
    push result, $P0
    goto it_loop
  it_loop_end:
    .return (result)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
