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
    .local pmc props, whence
    props = getattribute self, '%!properties'
    if null props goto ret_undef
    whence = props['WHENCE']
    if null whence goto ret_undef
    .return (whence)
  ret_undef:
    whence = new 'Undef'
    .return (whence)
.end

=back

=head2  Private methods

=over

=item !IMMUTABLE()

=item !MUTABLE()

Indicate that objects in the class are mutable or immutable.

=cut

.sub '!IMMUTABLE' :method
    $P0 = get_hll_global ['Int'], 'Scalar'
    $P1 = self.'HOW'()
    $P1.'add_method'('Scalar', $P0, 'to'=>self)
.end

.sub '!MUTABLE' :method
    $P0 = get_hll_global ['Perl6Object'], 'Scalar'
    $P1 = self.'HOW'()
    $P1.'add_method'('Scalar', $P0, 'to'=>self)
.end


=back

=head2 Vtable functions

=over

=item get_pmc_keyed(key)    (vtable method)

Returns a proto-object with an autovivification closure attached to it.

=cut

.sub get_pmc_keyed :vtable :method
    .param pmc what

    # We'll build auto-vivification hash of values.
    .local pmc WHENCE, key, val
    WHENCE = new 'Hash'

    # What is it?
    $S0 = what.'WHAT'()
    if $S0 == 'Pair' goto from_pair
    if $S0 == 'List' goto from_list
    'die'("Auto-vivification closure did not contain a Pair")

  from_pair:
    # Just a pair.
    key = what.'key'()
    val = what.'value'()
    WHENCE[key] = val
    goto done_whence

  from_list:
    # List.
    .local pmc list_iter, cur_pair
    list_iter = new 'Iterator', what
  list_iter_loop:
    unless list_iter goto done_whence
    cur_pair = shift list_iter
    key = cur_pair.'key'()
    val = cur_pair.'value'()
    WHENCE[key] = val
    goto list_iter_loop
  done_whence:

    # Now create a clone of the protoobject.
    .local pmc protoclass, res, props, tmp
    protoclass = class self
    res = new protoclass

    # Attach the WHENCE property.
    props = getattribute self, '%!properties'
    unless null props goto have_props
    props = new 'Hash'
  have_props:
    props['WHENCE'] = WHENCE
    setattribute res, '%!properties', props

    .return (res)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
