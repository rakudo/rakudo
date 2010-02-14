## $Id$

=head1 TITLE

Whatever - Perl 6 Whatever class

=head1 DESCRIPTION

This file implements the Whatever class.

=cut

.namespace ['Whatever']

.sub 'onload' :anon :init :load
    .local pmc p6meta, whateverproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    whateverproto = p6meta.'new_class'('Whatever', 'parent'=>'Failure')
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
