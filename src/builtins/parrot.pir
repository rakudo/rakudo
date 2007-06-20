## $Id: parrot.pir 18087 2007-04-09 22:02:45Z paultcochrane $

=head1 NAME

src/builtins/parrot.pir - Special-purpose functions for Parrot

=head1 Functions

=over 4

=item trace

=cut

.namespace

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
# vim: expandtab shiftwidth=4:
