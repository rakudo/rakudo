## $Id$

=head1 NAME

src/classes/Range.pir - methods for the Range class

=head1 Methods

=over 4

=cut

.namespace ['Range']

.sub 'onload' :anon :load :init
    .local pmc p6meta
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    p6meta.'new_class'('Range', 'parent'=>'Any', 'attr'=>'$!from $!to')
.end


=item from

Gets the value the range starts from.

=cut

.sub 'from' :method
    $P0 = getattribute self, '$!from'
    .return ($P0)
.end


=item to

Gets the value the range goes up to.

=cut

.sub 'to' :method
    $P0 = getattribute self, '$!to'
    .return ($P0)
.end


=item list

Gets the range in list context.

XXX For now we just produce a list of values in the range. When we go lazy,
we need to not do that.

=cut

.sub 'list' :method
    .local pmc res, cur_val
    res = new 'List'
  it_loop:
    unless self goto it_loop_end
    cur_val = shift self
    push res, cur_val
    goto it_loop
  it_loop_end:
    .return(res)
.end


=item perl

Returns a Perl code representation of the range.

=cut

.sub perl :method
    # Get to and from, and then their perl representations.
    $P0 = getattribute self, '$!from'
    $S0 = $P0.'perl'()
    $P1 = getattribute self, '$!to'
    $S1 = $P1.'perl'()

    # Generate to...from
    concat $S0, ".."
    concat $S0, $S1
    .return($S0)
.end


=item get_iter (vtable)

Just returns this Range itself, since a Range is an iterator.

=cut

.sub get_iter :method :vtable
    .return (self)
.end


=item shift_pmc (vtable)

Gets the next value from the iterator.

=cut

.sub shift_pmc :method :vtable
    .local pmc from_val, to_val, ret_val

    # Check we've still got values.
    from_val = getattribute self, '$!from'
    to_val = getattribute self, '$!to'
    if from_val > to_val goto no_more_elements

    # Update current position and return value.
    ret_val = 'postfix:++'(from_val)
    setattribute self, '$!from', from_val
    .return (ret_val)

    # If there's nothing more, return undef.
  no_more_elements:
    .local pmc proto
    proto = get_hll_global 'Failure'
    .return proto.'new'()
.end


=item get_bool (vtable)

Returns true if there are any more values to iterate over, and false otherwise.

=cut

.sub get_bool :method :vtable
    # Check we've still got values.
    .local pmc from_val, to_val
    from_val = getattribute self, '$!from'
    to_val = getattribute self, '$!to'
    $I0 = from_val <= to_val
    .return ($I0)
.end


=item defined (vtable)

Returns true if there are any more values to iterate over, and false otherwise.

=cut

.sub defined :method :vtable
    # Check we've still got values.
    .local pmc from_val, to_val
    from_val = getattribute self, '$!from'
    to_val = getattribute self, '$!to'
    $I0 = from_val <= to_val
    .return ($I0)
.end


=back

=head1 Operators

=over 4

=item infix:<..>

Constructs a range from the value on the LHS to the value on the RHS.

=cut

.namespace

.sub "infix:.."
    .param pmc a
    .param pmc b
    .local pmc proto
    proto = get_hll_global 'Range'
    .return proto.'new'('from' => a, 'to' => b)
.end


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
