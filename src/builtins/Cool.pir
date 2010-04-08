=head1 TITLE

Cool - Perl 6 Cool class

=head1 DESCRIPTION

This is a class that most builtin classes should inherit from

=cut

.namespace['Cool']
.sub 'onload' :anon :init :load
    .local pmc p6meta, proto
    p6meta = get_hll_global ['Mu'], '$!P6META'
    proto = p6meta.'new_class'('Cool', 'parent'=>'Any')
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:

