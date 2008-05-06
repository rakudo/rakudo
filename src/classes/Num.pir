## $Id$

=head1 TITLE

Num - Perl 6 numbers

=cut

.namespace [ 'Num' ]


=head1 SUBROUTINES

=over 4

=item onload()

=cut

.sub 'onload' :anon :init :load
    $P0 = subclass 'Float', 'Num'
    $P1 = get_hll_global 'Any'
    $P1 = $P1.HOW()
    addparent $P0, $P1
    $P1 = get_hll_global ['Perl6Object'], 'make_proto'
    $P1($P0, 'Num')
    $P1('Float', 'Num')
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


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
