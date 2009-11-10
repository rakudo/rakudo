## $Id$

=head1 TITLE

Iterator - Perl 6 iterator

=head1 SUBROUTINES

=over 4

=item onload

=cut

.namespace []
.sub 'onload' :anon :init :load
    .local pmc p6meta, iterproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    iterproto = p6meta.'new_class'('Perl6Iterator', 'parent'=>'Any')
    p6meta.'register'('Iterator', 'parent'=>iterproto, 'protoobject'=>iterproto)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
