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

=item name

=cut

.sub 'name' :method
    $S0 = self
    .return ($S0)
.end


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
    setattribute inner, ['Sub'], 'proxy', $P0
    $P1 = prophash self
    $P2 = iter $P1
  it_loop:
    unless $P2 goto it_loop_end
    $S0 = shift $P2
    $P3 = $P1[$S0]
    setprop inner, $S0, $P3
    goto it_loop
  it_loop_end:
    setprop $P0, '$!real_self', inner

    # Then assign the Parrot sub of the wrapper to ourself, and set the inner block
    # and handle as properties on ourself too.
    $P0 = getattribute wrapper, ['Sub'], 'proxy'
    setattribute self, ['Sub'], 'proxy', $P0
    setprop $P0, '$!real_self', self
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
    setattribute current, ['Sub'], 'proxy', $P1
    setprop $P1, '$!real_self', current
    $P1 = getprop '$!wrap_inner', $P0
    if null $P1 goto unwrap_done
    setprop current, '$!wrap_inner', $P1
    $P1 = getprop '$!wrap_handle', $P0
    setprop current, '$!wrap_handle', $P1
  unwrap_done:
    $P0 = new 'Nil'
    .return ($P0)

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
