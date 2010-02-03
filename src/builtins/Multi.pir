## $Id$

=head1 TITLE

Multi - Perl 6 multi-dispatch routine

=head1 SUBROUTINES

=over 4

=item onload()

=cut

.namespace [ 'Multi' ]

.sub 'onload' :anon :init :load
    .local pmc p6meta, proto
    p6meta = get_hll_global ['Mu'], '$!P6META'
    proto = p6meta.'new_class'('Multi', 'parent'=>'parrot;Perl6MultiSub Code')
    p6meta.'register'('Perl6MultiSub', 'parent'=>proto, 'protoobject'=>proto)
    p6meta.'register'('MultiSub', 'parent'=>proto, 'protoobject'=>proto)
.end

=item multi

=cut

.sub 'multi' :method
    $P0 = get_hll_global ['Bool'], 'True'
    .return ($P0)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
