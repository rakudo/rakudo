## $Id$

=head1 TITLE

Whatever - Perl 6 Whatever class

=head1 DESCRIPTION

This file implements the Whatever class.

=cut

.namespace ['Whatever']

.sub 'onload' :anon :init :load
    .local pmc p6meta, whateverproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    whateverproto = p6meta.'new_class'('Whatever', 'parent'=>'Perl6Object')
    whateverproto.'!IMMUTABLE'()
.end

.sub '' :vtable('get_integer') :method
    $P0 = get_global '$!slice'
    unless null $P0 goto have_whatever
    $P0 = 'undef'()
  have_whatever:
    $I0 = $P0
    .return ($I0)
.end

.sub '' :vtable('get_number') :method
    $P0 = get_global '$!slice'
    unless null $P0 goto have_whatever
    $P0 = 'undef'()
  have_whatever:
    $N0 = $P0
    .return ($N0)
.end


# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
