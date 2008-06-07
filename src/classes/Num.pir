## $Id$

=head1 TITLE

Num - Perl 6 numbers

=head1 SUBROUTINES

=over 4

=item onload()

=cut

.namespace [ 'Num' ]

.sub 'onload' :anon :init :load
    .local pmc p6meta, numproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    numproto = p6meta.'new_class'('Num', 'parent'=>'Float Any')
    p6meta.'register'('Float', 'parent'=>numproto, 'protoobject'=>numproto)

    $P0 = get_hll_namespace ['Num']
    '!EXPORT'('cis unpolar', 'from'=>$P0)
.end


=item ACCEPTS()

=cut

.sub 'ACCEPTS' :method
    .param num topic
    .return 'infix:=='(topic, self)
.end


=item perl()

Returns a Perl representation of the Num.

=cut

.sub 'perl' :method
    $S0 = self
    .return($S0)
.end


=item cis(angle)

=cut

.sub 'cis' :method
    .return 'unpolar'(1.0, self)
.end

=item unpolar(angle)

=cut

.sub 'unpolar' :method
    .param num angle
    .local num mag
    .local pmc result
    mag = self
    result = new 'Complex'
    $N0 = cos angle
    $N0 *= mag
    result[0] = $N0
    $N0 = sin angle
    $N0 *= mag
    result[1] = $N0
    .return (result)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
