## $Id$

=head1 TITLE

Subset - Class for defining a subset type

=head1 DESCRIPTION

This file contains a helper class for implementing subset types. It may or
may not turn out to be the right thing to do.

=cut

.namespace ['Subset']

.sub 'onload' :anon :init :load
    .local pmc p6meta
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    p6meta.'new_class'('Subset', 'parent'=>'Any', 'attr'=>'$!condition')
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
    .tailcall $P0(topic)
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
