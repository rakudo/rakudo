## $Id: $

=head1 TITLE

IO - Perl 6 IO class

=head1 DESCRIPTION

This file implements the IO file handle class.

=head1 Methods

=over 4

=cut

.namespace ['IO']

.sub 'onload' :anon :init :load
    $P0 = subclass 'Perl6Object', 'IO'
    addattribute $P0, "$!PIO" # for Parrot IO object
    $P1 = get_hll_global ['Perl6Object'], 'make_proto'
    $P1($P0, 'IO')
.end


=item print

Writes the given list of items to the file.

=cut

.sub 'print' :method
    .param pmc args            :slurpy
    .local pmc iter
    .local pmc PIO
    PIO = getattribute self, "$!PIO"
    args = 'list'(args)
    iter = new 'Iterator', args
  iter_loop:
    unless iter goto iter_end
    $S0 = shift iter
    print PIO, $S0
    goto iter_loop
  iter_end:
    .return (1)
.end


=item say

Writes the given list of items to the file, then a newline character.

=cut

.sub 'say' :method
    .param pmc list            :slurpy
    .local pmc PIO
    PIO = getattribute self, "$!PIO"
    self.'print'(list)
    print PIO, "\n"
    .return (1)
.end


=item close

Closes the file.

=cut

.sub 'close' :method
    .local pmc PIO
    PIO = getattribute self, "$!PIO"
    close PIO
    .return(1)
.end


=item get_iter (vtable)

Gets an iterator for this IO handle.

=cut

.sub 'get_iter' :method :vtable
    .local pmc PIO
    PIO = getattribute self, "$!PIO"
    $P0 = iter PIO
    .return($P0)
.end


=item say




=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
