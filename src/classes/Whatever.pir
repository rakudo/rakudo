## $Id$

=head1 TITLE

Whatever - Perl 6 Whatever class

=head1 DESCRIPTION

This file implements the Whatever class.

=cut

.namespace ['Whatever']

.sub 'onload' :anon :init :load
    $P0 = subclass 'Any', 'Whatever'
    $P1 = get_hll_global ['Perl6Object'], 'make_proto'
    $P1($P0, 'Whatever')
.end

.sub 'ACCEPTS' :method
    .param pmc topic
    $P0 = get_hll_global ['Bool'], 'True'
    .return ($P0)
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
