## $Id$

=head1 NAME

src/builtins/parrot.pir - Special-purpose functions for Parrot

=head1 Functions

=over 4

=item trace

=cut

.namespace []

.sub 'parrot_trace'
    .param int tracelevel
    trace tracelevel
    .return ()
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
