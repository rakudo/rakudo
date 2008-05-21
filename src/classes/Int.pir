## $Id$

=head1 TITLE

Int - Perl 6 integers

=cut

.namespace [ 'Int' ]


=head1 SUBROUTINES

=over 4

=item onload

=cut

.sub 'onload' :anon :init :load
    $P0 = subclass 'Integer', 'Int'
    $P1 = get_hll_global 'Any'
    $P1 = $P1.HOW()
    addparent $P0, $P1
    $P1 = get_hll_global ['Perl6Object'], 'make_proto'
    $P1($P0, 'Int')
    $P1('Integer', 'Int')
.end


=item ACCEPTS()

=cut

.sub 'ACCEPTS' :method
    .param num topic
    .return 'infix:=='(topic, self)
.end


=item clone()

=cut

.sub 'clone' :method :vtable
    .local int val
    $P0 = new 'Int'
    $I0 = self
    $P0 = $I0
    .return($P0)
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
