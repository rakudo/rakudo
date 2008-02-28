## $Id$

=head1 NAME

src/classes/Pair.pir - methods for the Pair class

=head1 Methods

=over 4

=cut

.namespace ['Pair']

.sub 'onload' :anon :load :init
    $P1 = get_hll_global ['Perl6Object'], 'make_proto'
    $P1('Pair', 'Pair')
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
