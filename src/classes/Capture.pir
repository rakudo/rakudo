## $Id$

=head1 TITLE

Capture - Perl 6 Capture class

=head1 DESCRIPTION

This file sets up the Perl 6 C<Capture> class.

=cut

.namespace ['Perl6Capture']

.sub 'onload' :anon :init :load
    load_bytecode 'Parrot/Capture_PIR.pbc'
    .local pmc p6meta, captureproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    captureproto = p6meta.'new_class'('Perl6Capture', 'parent'=>'Capture Any', 'name'=>'Capture')
    captureproto.'!IMMUTABLE'()
.end


=head2 Methods

=over 4

=item get_string()   (vtable)

=cut

.sub '' :vtable('get_string') :method
    $S0 = self.'item'()
    .return ($S0)
.end

.sub '' :vtable('get_number') :method
    $N0 = self.'item'()
    .return ($N0)
.end

.sub 'item' :method
    $P0 = self[0]
    unless null $P0 goto end
    $P0 = 'undef'()
  end:
    .return ($P0)
.end


=back

=head2 Operators

=over 4

=item prefix:<\\>

Build a capture from its argument(s).

=cut

.namespace []
.sub "prefix:\\"
    .param pmc arg
    $I0 = isa arg, 'ObjectRef'
    if $I0 goto have_ref
    arg = new 'ObjectRef', arg
  have_ref:
    .return (arg)
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
