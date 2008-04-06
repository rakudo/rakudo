## $Id$

=head1 TITLE

Any - Perl 6 Any class

=head1 DESCRIPTION

This file implements the Any class.

=cut

.namespace [ 'Any' ]

.sub 'onload' :anon :init :load
    $P0 = subclass 'Perl6Object', 'Any'
    $P1 = get_hll_global ['Perl6Object'], 'make_proto'
    $P1($P0, 'Any')
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
