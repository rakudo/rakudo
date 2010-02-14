## $Id$

=head1 TITLE

Capture - Perl 6 Capture class

=head1 DESCRIPTION

This file sets up the Perl 6 C<Capture> class.

=cut

.namespace ['Perl6Capture']

.sub 'onload' :anon :init :load
    .local pmc p6meta, captureproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    captureproto = p6meta.'new_class'('Perl6Capture', 'parent'=>'parrot;Capture Any', 'name'=>'Capture')
    captureproto.'!IMMUTABLE'()
.end


=head2 Methods

=over 4

=item new

Turns the positional arguments into the capture's positionals, and the named
arguments into the capture's nameds.

=cut

.sub 'new' :method
    .param pmc pos_args   :slurpy
    .param pmc named_args :slurpy :named
    
    .local pmc it, result
    result = new ['Perl6Capture']
    it = iter pos_args
  it_pos_loop:
    unless it goto it_pos_loop_end
    $P0 = shift it
    push result, $P0
    goto it_pos_loop
  it_pos_loop_end:
    it = iter named_args
  it_named_loop:
    unless it goto it_named_loop_end
    $S0 = shift it
    $P0 = named_args[$S0]
    result[$S0] = $P0
    goto it_named_loop
  it_named_loop_end:

    .return (result)
.end


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
.sub 'prefix:\'
    .param pmc arg
    $I0 = isa arg, 'Perl6Scalar'
    if $I0 goto have_ref
    arg = root_new ['parrot';'Perl6Scalar'], arg
  have_ref:
    .return (arg)
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
