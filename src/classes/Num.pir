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
    numproto.'!IMMUTABLE'()
    p6meta.'register'('Float', 'parent'=>numproto, 'protoobject'=>numproto)

    # Override the proto's ACCEPT method so we also accept Ints.
    .const 'Sub' $P0 = "Num::ACCEPTS"
    $P1 = typeof numproto
    $P1.'add_method'('ACCEPTS', $P0)
.end


.sub 'Num::ACCEPTS' :anon :method
    .param pmc topic

    ##  first, try our superclass .ACCEPTS
    $P0 = get_hll_global 'Any'
    $P1 = find_method $P0, 'ACCEPTS'
    $I0 = self.$P1(topic)
    unless $I0 goto try_int
    .return ($I0)

  try_int:
    $P0 = get_hll_global 'Int'
    $I0 = $P0.'ACCEPTS'(topic)
    .return ($I0)
.end


=item ACCEPTS()

=cut

.sub 'ACCEPTS' :method
    .param num topic
    $S0 = self
    if $S0 == 'NaN' goto test_nan
    .tailcall 'infix:=='(topic, self)
  test_nan:
    $S0 = topic
    $I0 = iseq $S0, 'NaN'
    .tailcall 'prefix:?'($I0)
.end


=item perl()

Returns a Perl representation of the Num.

=cut

.sub 'perl' :method
    $S0 = self
    .return($S0)
.end


=item succ and pred

Increment and Decrement Methods

=cut

.sub 'pred' :method
    $P0 = clone self
    dec $P0
    .return ($P0)
.end

.sub 'succ' :method
    $P0 = clone self
    inc $P0
    .return ($P0)
.end


=item WHICH()

Returns the identify value.

=cut

.sub 'WHICH' :method
    $N0 = self
    .return ($N0)
.end


=item infix:===

Overridden for Num.

=cut

.namespace []
.sub 'infix:===' :multi(Float,Float)
    .param num a
    .param num b
    .tailcall 'infix:=='(a, b)
.end


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
