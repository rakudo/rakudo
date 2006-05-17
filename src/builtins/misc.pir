## $Id: builtins.pir 12709 2006-05-17 01:42:08Z pmichaud $

=head1 NAME

src/builtins/misc.pir - Perl 6 miscellaneous builtins

=head1 Functions

=over 4

=cut

.namespace [ "" ]

=item C<infix:,(...)>

Builds an array from its arguments.  Trivial, really.

=cut

.sub 'infix:,'
    .param pmc list            :slurpy
    .return (list)
.end


.sub 'die'
    .param pmc list            :slurpy
    .local pmc iter
    .local string message

    message = ''
    iter = new .Iterator, list
  iter_loop:
    unless iter goto iter_end
    $P0 = shift iter
    $S0 = $P0
    message .= $S0
    goto iter_loop
  iter_end:
    $P0 = new .Exception
    $P0['_message'] = message
    throw $P0
    .return ()
.end

=back

=cut


## vim: expandtab sw=4
