## $Id$

=head1 TITLE

Int - Perl 6 integers

=head1 SUBROUTINES

=over 4

=item onload

=cut

.namespace [ 'Int' ]

.sub 'onload' :anon :init :load
    .local pmc p6meta, intproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    intproto = p6meta.'new_class'('Int', 'parent'=>'Integer Any')
    p6meta.'register'('Integer', 'parent'=>intproto, 'protoobject'=>intproto)
.end


=item ACCEPTS()

=cut

.sub 'ACCEPTS' :method
    .param num topic
    .return 'infix:=='(topic, self)
.end


=item perl()

Returns a Perl representation of the Int.

=cut

.sub 'perl' :method
    $S0 = self
    .return($S0)
.end


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
