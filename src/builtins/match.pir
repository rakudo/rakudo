## $Id$

=head1 NAME

src/builtins/match.pir - Perl6 builtins for smart matching

=head1 Functions

=over 4

=cut

.namespace

.sub 'infix:~~'
    .param pmc topic
    .param pmc x
    .return x.ACCEPTS(topic)
.end


.sub 'infix:!~~'
    .param pmc topic
    .param pmc x
    .return x.REJECTS(topic)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
