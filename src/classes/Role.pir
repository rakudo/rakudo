## $Id$

=head1 NAME

src/classes/Role.pir - methods for the Role class

=head1 Methods

=over 4

=cut

.namespace ['Role']

=item ACCEPTS(topic)

=cut

.sub 'ACCEPTS' :method
    .param pmc topic
    $I0 = does topic, self
    .return 'prefix:?'($I0)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
