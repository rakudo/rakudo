## $Id$

=head1 TITLE

Capture - Perl 6 Capture class

=head1 DESCRIPTION

This file sets up the Perl 6 C<Capture> class.

=cut

.namespace ['Perl6Capture']

.sub 'onload' :anon :init :load
    $P0 = subclass 'Any', 'Perl6Capture'
    addattribute $P0, '$!scalar'
    addattribute $P0, '@!array'
    addattribute $P0, '%!hash'
    $P1 = get_hll_global ['Perl6Object'], 'make_proto'
    $P1($P0, 'Capture')
.end


=head1 METHODS

=over

=item !create

Creates a capture.

=cut

.sub '!create' :method
    .param pmc invocant
    .param pmc array :slurpy
    .param pmc hash :named :slurpy

    # Create capture and set parts of it.
    .local pmc capt
    capt = self.'new'()
    setattribute capt, '$!scalar', invocant
    setattribute capt, '@!array', array
    setattribute capt, '%!hash', hash

    # Done.
    .return(capt)
.end


=item clone (vtable method)

Captures are immutable, so just return ourself.

=cut

.sub 'clone' :method :vtable
    .return (self)
.end


=item get_pmc_keyed (vtable method)

Gets the given item from the capture.

XXX Contains workaround until we get keyed_int in place in PCT.

=cut

.sub 'get_pmc_keyed' :vtable :method
    .param pmc key
    $I0 = isa key, 'Integer'
    if $I0 goto int_key

  hash_key:
    $P0 = getattribute self, '%!hash'
    $P0 = $P0[key]
    .return ($P0)
    
  int_key:
    $P0 = getattribute self, '@!array'
    $P0 = $P0[key]
    .return ($P0)
.end


=item item (method)

Gets the invocant part of the capture.

=cut

.sub 'item' :method
    $P0 = getattribute self, '$!scalar'
    .return ($P0)
.end


=item list (method)

Gets the positional part of the capture.

=cut

.sub 'list' :method
    $P0 = getattribute self, '@!array'
    .return ($P0)
.end


=item hash (method)

Gets the named part of the capture.

=cut

.sub 'hash' :method
    $P0 = getattribute self, '%!hash'
    .return ($P0)
.end


=back

=cut


# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
