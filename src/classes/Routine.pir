## $Id$

=head1 TITLE

Code - Perl 6 Routine class

=head1 DESCRIPTION

This file sets up the Perl 6 C<Routine> class, the base class for all
wrappable executable objects.

=cut

.namespace ['Routine']

.sub 'onload' :anon :load :init
    .local pmc p6meta
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    p6meta.'new_class'('Routine', 'parent'=>'Block')
.end


=head1 METHODS

=over 4

=item wrap

=cut

.sub 'wrap' :method
    .param pmc wrapper

    # Did we already wrap? If so, get handle and increment it to make a new
    # one; otherwise, start from 1.
    .local pmc handle
    handle = getprop '$!wrap_handle', self
    unless null handle goto have_handle
    handle = box 0
  have_handle:
    handle = 'infix:+'(handle, 1)

    # Take current Parrot-level sub and re-bless it into a block (so CALLER
    # won't see it as a routine). Copy properties.
    .local pmc inner
    inner = get_hll_global 'Block'
    inner = inner.'new'()
    $P0 = getattribute self, ['Sub'], 'proxy'
    assign inner, $P0
    $P0 = prophash self
    $P1 = iter $P0
  it_loop:
    unless $P1 goto it_loop_end
    $S0 = shift $P1
    $P2 = $P0[$S0]
    setprop inner, $S0, $P2
    goto it_loop
  it_loop_end:

    # Then assign the Parrot sub of the wrapper to ourself, and set the inner block
    # and handle as properties on ourself too.
    $P0 = getattribute wrapper, ['Sub'], 'proxy'
    assign self, $P0
    setprop self, '$!wrap_handle', handle
    setprop self, '$!wrap_inner', inner

    .return (handle)
.end


=item unwrap

=cut

.sub 'unwrap' :method
    .param pmc handle

    # Search for wrap handle.
    .local pmc current
    current = self
  search_loop:
    $P0 = getprop '$!wrap_handle', current
    if null $P0 goto handle_not_found
    if $P0 == handle goto found
    current = getprop '$!wrap_inner', current
    goto search_loop

    # If found, unwrap and fix up chain to eliminate now-unused sub.
  found:
    $P0 = getprop '$!wrap_inner', current
    $P1 = getattribute $P0, ['Sub'], 'proxy'
    assign current, $P1
    $P1 = getprop '$!wrap_inner', $P0
    if null $P1 goto unwrap_done
    setprop current, '$!wrap_inner', $P1
    $P1 = getprop '$!wrap_handle', $P0
    setprop current, '$!wrap_handle', $P1
  unwrap_done:
    .return ()

  handle_not_found:
    'die'('Could not find unwrap handle ', handle, ' on sub ', self)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
