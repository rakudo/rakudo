=head1 TITLE

IterDone - list iterator sentinel

=head1 DESCRIPTION

IterDone acts as the "end of list" sentinel for
iterations.  

=head2 Methods

=over 4

=cut

.namespace ['IterDone']
.sub 'onload' :anon :init :load
    .local pmc p6meta, proto
    p6meta = get_hll_global ['Mu'], '$!P6META'
    proto = p6meta.'new_class'('IterDone', 'parent'=>'Any')
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
