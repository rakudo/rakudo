## $Id$

=head1 NAME

src/classes/Sub.pir - methods for the Sub class

=head1 Methods

=over 4

=cut

.namespace ['Sub']

.sub 'onload' :anon :load :init
    $P1 = get_hll_global ['Perl6Object'], 'make_proto'
    $P1('Sub', 'Sub')
.end

=item ACCEPTS(topic)

=cut

.sub 'ACCEPTS' :method
    .param pmc topic
    .local pmc match
    match = self(topic)
    $P0 = getinterp
    $P1 = $P0['lexpad';1]
    $P1['$/'] = match
    .return (match)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
