## $Id$

=head1 TITLE

Capture - Perl 6 Capture class

=head1 DESCRIPTION

This file sets up the Perl 6 C<Capture> class.

=cut

.namespace ['Perl6Capture']

.sub 'onload' :anon :init :load
    load_bytecode 'PCT.pbc'
    .local pmc p6meta, captureproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    captureproto = p6meta.'new_class'('Perl6Capture', 'parent'=>'Capture_PIR Any', 'name'=>'Capture')
.end


=item Scalar

This is a value type, so just returns itself.

=cut

.sub 'Scalar' :method
    .return (self)
.end


=head2 Methods

=over 4

=item get_string()   (vtable)

=cut

.sub 'VTABLE_get_string' :method :vtable('get_string')
    $S0 = self.'list'()
    .return ($S0)
.end


=back

=head2 Operators

=over 4

=item prefix:<\\>

Build a capture from its argument(s).

=cut

.namespace []
.sub "prefix:\\"
    .param pmc list            :slurpy
    .param pmc hash            :slurpy :named
    .local pmc result, item
    result = new 'Perl6Capture'
    setattribute result, '@!list', list
    item = list
    $I0 = list.'elems'()
    if $I0 != 1 goto item_done
    item = item[0]
    item = item.'item'()
  item_done:
    setattribute result, '$!item', item
    .local pmc it
    it = iter hash
  hash_loop:
    unless it goto hash_end
    $S0 = shift it
    $P0 = hash[$S0]
    result[$S0] = $P0
    goto hash_loop
  hash_end:
    .return (result)
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
