## $Id:$

=head1 TITLE

Subset - Class for defining a subset type

=head1 DESCRIPTION

This file contains a helper class for implementing subset types. It may or
may not turn out to be the right thing to do.

=cut

.namespace ['Subset']

.sub 'onload' :anon :init :load
    $P0 = subclass 'Any', 'Subset'
    addattribute $P0, '$!condition'
    $P1 = get_hll_global ['Perl6Object'], 'make_proto'
    $P1($P0, 'Subset')
.end

.sub '!create' :method
    .param string name
    .param pmc condition
    .local pmc subset
    subset = self.'new'('condition' => condition)
    set_hll_global name, subset
.end

.sub 'ACCEPTS' :method
    .param pmc topic
    $P0 = getattribute self, '$!condition'
    .return $P0(topic)
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
