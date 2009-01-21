## $Id$

=head1 TITLE

Protoobject - methods on Protoobjects

=head1 DESCRIPTION

=head2 Methods on P6protoobject

=over

=item defined()

=cut

.namespace ['P6protoobject']
.sub 'defined' :method
    $P0 = get_hll_global ['Bool'], 'False'
    .return ($P0)
.end


=item perl()

Returns a Perl representation of itself.

=cut

.sub 'perl' :method
    $S0 = self
    .return ($S0)
.end

=item WHENCE()

Returns the protoobject's autovivification closure.

=cut

.namespace ['P6protoobject']
.sub 'WHENCE' :method
    .local pmc whence
    whence = getprop '%!WHENCE', self
    unless null whence goto done
    whence = new 'Undef'
  done:
    .return (whence)
.end

=back

=head2 Functions

=over

=item postcircumfix:<{ }>

Return a clone of the protoobject with a new WHENCE property set.

=cut

.namespace ['P6protoobject']
.sub 'postcircumfix:{ }' :method
    .param pmc WHENCE :slurpy :named
    .local pmc protoclass, proto
    protoclass = typeof self
    proto = new protoclass
    setprop proto, '%!WHENCE', WHENCE
    .return (proto)
.end


=back

=head2  Coercions

=over

=item Scalar()

=cut

.namespace ['P6protoobject']
.sub 'Scalar' :method
    .return (self)
.end

=back

=head2  Private methods

=over

=item !flatten()

=cut

.namespace ['P6protoobject']
.sub '!flatten' :method
    $P0 = new 'ResizablePMCArray'
    push $P0, self
    .return ($P0)
.end

=item !IMMUTABLE()

=item !MUTABLE()

Indicate that objects in the class are mutable or immutable.

=cut

.namespace ['P6protoobject']
.sub '!IMMUTABLE' :method
    $P0 = get_hll_global ['Int'], 'Scalar'
    $P1 = self.'HOW'()
    $P1.'add_method'('Scalar', $P0, 'to'=>self)
.end

.namespace ['P6protoobject']
.sub '!MUTABLE' :method
    $P0 = get_hll_global ['Perl6Object'], 'Scalar'
    $P1 = self.'HOW'()
    $P1.'add_method'('Scalar', $P0, 'to'=>self)
.end

=back

=head2 Vtable functions

=cut

.namespace ['P6protoobject']
.sub '' :vtable('get_bool') :method
    $P0 = '!FAIL'('Use of protoobject as value')
    $I0 = istrue $P0
    .return ($I0)
.end

.namespace ['P6protoobject']
.sub '' :vtable('get_integer') :method
    $P0 = '!FAIL'('Use of protoobject as value')
    $I0 = $P0
    .return ($I0)
.end

.namespace ['P6protoobject']
.sub '' :vtable('get_number') :method
    $P0 = '!FAIL'('Use of protoobject as value')
    $N0 = $P0
    .return ($N0)
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
