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
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    proto = p6meta.'new_class'('Multi', 'parent'=>'parrot;Perl6MultiSub Code Any')
    p6meta.'register'('Perl6MultiSub', 'parent'=>proto, 'protoobject'=>proto)
    p6meta.'register'('MultiSub', 'parent'=>proto, 'protoobject'=>proto)
.end

.sub 'Scalar' :method
    .return (self)
.end


.namespace []

.sub 'prefix:~' :multi('MultiSub')
    .param pmc multi
    .tailcall multi.'name'()
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
