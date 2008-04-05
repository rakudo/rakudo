## $Id:$

=head1 TITLE

Capture - Perl 6 Capture class

=head1 DESCRIPTION

This file sets up the Perl 6 C<Capture> class.

=cut

.namespace ['Capture']

.sub 'onload' :anon :init :load
    $P1 = get_hll_global ['Perl6Object'], 'make_proto'
    $P1('Capture', 'Capture')
.end

.sub '!create' :method
    .param pmc invocant
    .param pmc array :slurpy
    .param pmc hash :named :slurpy
    
    # Create capture.
    .local pmc capt
    capt = self.'new'()

    # Set array part.
    .local pmc it
    it = iter array
  array_loop:
    unless it goto array_loop_end
    $P0 = shift it
    push capt, $P0
    goto array_loop
  array_loop_end:

    # Set hash part.
    it = iter hash
  hash_loop:
    unless it goto hash_loop_end
    $P0 = shift it
    $P1 = hash[$P0]
    capt[$P0] = $P1
    goto hash_loop
  hash_loop_end:

    # Done.
    .return(capt)
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
