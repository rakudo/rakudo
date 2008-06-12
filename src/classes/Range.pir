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
    p6meta.'new_class'('Range', 'parent'=>'Any', 'attr'=>'$!from $!to $!from_exclusive $!to_exclusive')
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
    .local pmc res, cur_val, it
    it = clone self
    res = new 'List'
  it_loop:
    unless it goto it_loop_end
    cur_val = shift it
    push res, cur_val
    goto it_loop
  it_loop_end:
    .return(res)
.end


=item ACCEPTS(topic)

Smart-matches the given topic against the range.

=cut

.sub 'ACCEPTS' :method
    .param pmc topic

    # Get the range of values to test and exclusivity of endpoints.
    .local pmc from, to, from_exc, to_exc
    from = self.'from'()
    to = self.'to'()
    from_exc = getattribute self, "$!from_exclusive"
    to_exc = getattribute self, "$!to_exclusive"

    # Do test, depending on exclusivity of endpoints.
    if from_exc goto from_excluded
    if to_exc goto to_excluded
  inclusive:
    lt topic, from, false
    gt topic, to, false
    goto true
  from_excluded:
    if to_exc goto both_excluded
    le topic, from, false
    gt topic, to, false
    goto true
  to_excluded:
    lt topic, from, false
    ge topic, to, false
    goto true
  both_excluded:
    le topic, from, false
    ge topic, to, false

  true:
    $P0 = get_hll_global [ 'Bool' ], 'True'
    .return ($P0)
  false:
    $P0 = get_hll_global [ 'Bool' ], 'False'
    .return ($P0)
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

    # Generate to...from (with exclusive endpoints as needed).
    .local pmc from_exc, to_exc
    from_exc = getattribute self, "$!from_exclusive"
    to_exc = getattribute self, "$!to_exclusive"
    if from_exc goto from_excluded
    if to_exc goto to_excluded
  inclusive:
    concat $S0, ".."
    goto add_to
  from_excluded:
    if to_exc goto both_excluded
    concat $S0, "^.."
    goto add_to
  to_excluded:
    concat $S0, "..^"
    goto add_to
  both_excluded:
    concat $S0, "^..^"

  add_to:
    concat $S0, $S1
    .return($S0)
.end


=item iterator()   (vtable 'get_iter')

Since Range is already an iterator, just return a clone
of self.

=cut

.sub 'iterator' :method :vtable('get_iter')
    $P0 = clone self
    .return ($P0)
.end

=item clone (vtable)

Clone it self

=cut

.sub 'clone' :method :vtable
    .local pmc from, to, from_exc, to_exc, retv

    # Get to and from values.
    $P0 = self.'from'()
    from = clone $P0
    $P0 = self.'to'()
    to = clone $P0

    # What sort of range?
    from_exc = getattribute self, "$!from_exclusive"
    to_exc = getattribute self, "$!to_exclusive"
    if from_exc goto from_excluded
    if to_exc goto to_excluded
  inclusive:
    retv = 'infix:..'(from, to)
    .return (retv)
  from_excluded:
    if to_exc goto both_excluded
    retv = 'infix:^..'(from, to)
    .return (retv)
  to_excluded:
    retv = 'infix:..^'(from, to)
    .return (retv)
  both_excluded:
    retv = 'infix:^..^'(from, to)
    .return (retv)
.end


=item shift_pmc (vtable)

Gets the next value from the iterator.

=cut

.sub shift_pmc :method :vtable
    .local pmc from_val, to_val, from_exc, to_exc, end_check, ret_val
    from_val = getattribute self, '$!from'
    from_exc = getattribute self, '$!from_exclusive'
    to_val = getattribute self, '$!to'
    to_exc = getattribute self, '$!to_exclusive'

    # Need to handle differently depending on exclusive or inclusive.
    end_check = clone to_val
    unless to_exc goto to_inclusive
    'postfix:--'(end_check)
  to_inclusive:
    if from_exc goto from_exclusive
  from_inclusive:
    if from_val > end_check goto no_more_elements
    ret_val = 'postfix:++'(from_val)
    setattribute self, '$!from', from_val
    .return (ret_val)
  from_exclusive:
    if from_val >= end_check goto no_more_elements
    ret_val = 'prefix:++'(from_val)
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
    .local pmc from_val, to_val, from_exc, to_exc, end_check
    from_val = getattribute self, '$!from'
    from_exc = getattribute self, '$!from_exclusive'
    to_val = getattribute self, '$!to'
    to_exc = getattribute self, '$!to_exclusive'

    # Check we've still got values, sensitive to exclusive or inclusive endpoints.
    end_check = clone to_val
    unless to_exc goto to_inclusive
    'postfix:--'(end_check)
  to_inclusive:
    if from_exc goto from_exclusive
  from_inclusive:
    $I0 = from_val <= end_check
    .return ($I0)
  from_exclusive:
    $I0 = from_val < end_check
    .return ($I0)
.end


=item defined (vtable)

Returns true if there are any more values to iterate over, and false otherwise.

=cut

.sub defined :method :vtable
    # Just the same as bool_true.
    .return self.'true'()
.end


=back

=head1 Operators

=over 4

=item infix:<..>

Constructs a range from the value on the LHS to the value on the RHS.

=cut

.namespace []

.sub "infix:.."
    .param pmc a
    .param pmc b
    .local pmc proto
    proto = get_hll_global 'Range'
    .return proto.'new'('from' => a, 'to' => b)
.end


=item infix:<^..>

Constructs a range from the value on the LHS to the value on the RHS, where
the from value is excluded from the range.

=cut

.sub "infix:^.."
    .param pmc a
    .param pmc b
    .local pmc proto, true
    proto = get_hll_global 'Range'
    true = get_hll_global [ 'Bool' ], 'True'
    .return proto.'new'('from' => a, 'to' => b, 'from_exclusive' => true)
.end


=item infix:<..^>

Constructs a range from the value on the LHS to the value on the RHS, where
the to value is excluded from the range.

=cut

.sub "infix:..^"
    .param pmc a
    .param pmc b
    .local pmc proto, true
    proto = get_hll_global 'Range'
    true = get_hll_global [ 'Bool' ], 'True'
    .return proto.'new'('from' => a, 'to' => b, 'to_exclusive' => true)
.end


=item infix:<^..^>

Constructs a range from the value on the LHS to the value on the RHS, where
the from and to values are excluded from the range.

=cut

.sub "infix:^..^"
    .param pmc a
    .param pmc b
    .local pmc proto
    proto = get_hll_global 'Range'
    .return proto.'new'('from' => a, 'to' => b, 'from_exclusive' => 1, 'to_exclusive' => 1)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
