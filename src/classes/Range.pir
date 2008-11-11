## $Id$

=head1 NAME

src/classes/Range.pir - methods for the Range class

=head1 DESCRIPTION

=head2 Methods

=over 4

=cut

.namespace ['Range']

.sub 'onload' :anon :load :init
    .local pmc p6meta
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    p6meta.'new_class'('Range', 'parent'=>'Any', 'attr'=>'$!from $!to $!from_exclusive $!to_exclusive')
.end


=item Scalar

This is a value type, so just returns itself.

=cut

.sub 'Scalar' :method
    .return (self)
.end


=item VTABLE_get integer (vtable method)

=item VTABLE_get_number (vtable method)

=item VTABLE_get_string (vtable method)

=cut

.sub 'VTABLE_get_integer' :method :vtable('get_integer')
    $P0 = self.'list'()
    $I0 = $P0
    .return ($I0)
.end

.sub 'VTABLE_get_number' :method :vtable('get_number')
    $P0 = self.'list'()
    $N0 = $P0
    .return ($N0)
.end

.sub 'VTABLE_get_string' :method :vtable('get_string')
    $P0 = self.'list'()
    $S0 = $P0
    .return ($S0)
.end


=item ACCEPTS(topic)

Determines if topic is within the range or equal to the range.

=cut

.sub 'ACCEPTS' :method
    .param pmc topic

    $I0 = isa topic, 'Range'
    unless $I0 goto value_in_range_check
    $I0 = self.'from'()
    $I1 = topic.'from'()
    if $I0 != $I1 goto false
    $I0 = self.'to'()
    $I1 = topic.'to'()
    if $I0 != $I1 goto false
    $P0 = getattribute self, "$!from_exclusive"
    $P1 = getattribute topic, "$!from_exclusive"
    if $P0 != $P1 goto false
    $P0 = getattribute self, "$!to_exclusive"
    $P1 = getattribute topic, "$!to_exclusive"
    if $P0 != $P1 goto false
    goto true

  value_in_range_check:
    $I0 = self.'!from_test'(topic)
    unless $I0 goto false
    $I0 = self.'!to_test'(topic)
    unless $I0 goto false

  true:
    $P0 = get_hll_global ['Bool'], 'True'
    .return ($P0)
  false:
    $P0 = get_hll_global ['Bool'], 'False'
    .return ($P0)
.end


=item clone()   (vtable method)

Create a clone of the Range.

=cut

.sub 'clone' :method :vtable
     $P0 = self.'!cloneattr'('$!from $!to $!from_exclusive $!to_exclusive')
     .return ($P0)
.end


=item from()

=item to()

Gets the beginning or end of the range.

=cut

.sub 'from' :method
    $P0 = getattribute self, '$!from'
    .return ($P0)
.end

.sub 'to' :method
    $P0 = getattribute self, '$!to'
    .return ($P0)
.end


=item iterator()  (vtable method)

Return an iterator for the Range.  Since Ranges are already
iterators, we can just return a clone.

=cut

.sub 'iterator' :method :vtable('get_iter')
    $P0 = clone self
    .return ($P0)
.end


=item list()

Generate the Range in list context.  Currently we generate all
of the elements in the range; when we have lazy lists we can
just return a clone of the Range.

=cut

.sub 'list' :method
    .local pmc range_it, result
    range_it = self.'iterator'()
    result = new 'List'
  range_loop:
    unless range_it goto range_end
    $P0 = shift range_it
    push result, $P0
    goto range_loop
  range_end:
    .return (result)
.end


=item min()

=item minmax()

=item max()

=cut

.namespace ['Range']

.sub 'min' :method
    .tailcall self.'from'()
.end

.sub 'minmax' :method
    $P0 = self.'from'()
    $P1 = self.'to'()
    $P2 = get_hll_global 'list'
    .tailcall $P2($P0, $P1)
.end

.sub 'max' :method
    .tailcall self.'to'()
.end


=item pop()  (vtable_method)

Generate the next element at the end of the Range.

=cut

.sub 'pop' :method :vtable('pop_pmc')
    .local pmc to, toexc, value
    to = getattribute self, '$!to'
    toexc = getattribute self, '$!to_exclusive'
    value = 'postfix:--'(to)
    unless toexc goto have_value
    value = clone to
  have_value:
    $I0 = self.'!from_test'(value)
    if $I0 goto success
    value = '!FAIL'('Undefined value popped from empty range')
  success:
    .return (value)
.end


=item shift()   (vtable_method)

Generate the next element at the front of the Range.

=cut

.sub 'shift' :method :vtable('shift_pmc')
    .local pmc from, fromexc, value
    from = getattribute self, '$!from'
    fromexc = getattribute self, '$!from_exclusive'
    value = 'postfix:++'(from)
    unless fromexc goto have_value
    value = clone from
  have_value:
    $I0 = self.'!to_test'(value)
    if $I0 goto success
    value = '!FAIL'('Undefined value shifted from empty range')
  success:
    .return (value)
.end


=item true()

Return true if there are any more values to iterate over.

=cut

.sub 'true' :method :vtable('get_bool')
    .local pmc from, fromexc
    from = getattribute self, '$!from'
    fromexc = getattribute self, '$!from_exclusive'
    unless fromexc goto have_value
    from = clone from
    'postfix:++'(from)
  have_value:
    $I0 = self.'!to_test'(from)
    .return ($I0)
.end


=item perl()

Returns a Perl representation of the Range.

=cut

.sub 'perl' :method
    .local string result, tmp
    .local pmc from, fromexc, toexc, to
    from = getattribute self, '$!from'
    fromexc = getattribute self, '$!from_exclusive'
    toexc = getattribute self, '$!to_exclusive'
    to = getattribute self, '$!to'
    result = from.'perl'()
    unless fromexc goto dots
    result .= '^'
  dots:
    result .= '..'
    unless toexc goto end
    result .= '^'
  end:
    tmp = to.'perl'()
    result .= tmp
    .return (result)
.end

=back

=head2 Operators

=over 4

=item infix:<..>

=item infix:<^..>

=item infix:<..^>

=item infix:<^..^>

Construct a range from the endpoints.

=cut

.namespace []
.sub 'infix:..'
    .param pmc from
    .param pmc to
    .local pmc proto
    proto = get_hll_global 'Range'
    .tailcall proto.'new'('from'=>from, 'to'=>to)
.end

.sub 'infix:^..'
    .param pmc from
    .param pmc to
    .local pmc proto, true
    proto = get_hll_global 'Range'
    true = get_hll_global ['Bool'], 'True'
    .tailcall proto.'new'('from'=>from, 'to'=>to, 'from_exclusive'=>true)
.end

.sub 'infix:..^'
    .param pmc from
    .param pmc to
    .local pmc proto, true
    proto = get_hll_global 'Range'
    true = get_hll_global ['Bool'], 'True'
    .tailcall proto.'new'('from'=>from, 'to'=>to, 'to_exclusive'=>true)
.end

.sub 'infix:^..^'
    .param pmc from
    .param pmc to
    .local pmc proto, true
    proto = get_hll_global 'Range'
    true = get_hll_global ['Bool'], 'True'
    .tailcall proto.'new'('from'=>from, 'to'=>to, 'from_exclusive'=>true, 'to_exclusive'=>true)
.end

=item prefix:<^>(Any $to)

Construct a Range from C< 0 ..^ $to >.

=cut

.namespace[]
.sub 'prefix:^' :multi(_)
    .param pmc to
    .tailcall 'infix:..^'(0, to)
.end

=item prefix:<^>(Type $x)

Return $x.HOW.

=cut

.sub 'prefix:^' :multi('P6Protoobject')
    .param pmc proto
    .tailcall proto.'HOW'()
.end

=item prefix:<^>(List @a)

=cut

.sub 'prefix:^' :multi('ResizablePMCArray')
    .param pmc list

    # Iterate over the list and and create a list of Ranges
    .local pmc ranges, it
    ranges = 'list'()
    it = list.'iterator'()
  iter_loop:
    unless it goto iter_loop_end
    $P0 = shift it
    $P0 = 'infix:..^'(0, $P0)
    push ranges, $P0
    goto iter_loop
  iter_loop_end:

    # Now just use cross operator to make all the permutations.
    .tailcall 'infix:X'(ranges)
.end

=back

=head2 Private methods

=over 4

=item !from_test(topic)

=item !to_test(topic)

Returns true if C<topic> is greater than C<.from> / less than C<.to>,
honoring exclusive flags.

=cut

.namespace ['Range']
.sub '!from_test' :method
    .param pmc topic
    .local pmc from, fromexc
    from = getattribute self, '$!from'
    fromexc = getattribute self, '$!from_exclusive'
    if fromexc goto exclusive_test
    $I0 = isge topic, from
    .return ($I0)
  exclusive_test:
    $I0 = isgt topic, from
    .return ($I0)
.end

.sub '!to_test' :method
    .param pmc topic
    .local pmc to, toexc
    to = getattribute self, '$!to'
    $I0 = isa to, 'String'
    unless $I0 goto test_value
    $S0 = topic
    $I0 = length $S0
    $S1 = to
    $I1 = length $S1
    eq $I0, $I1, test_value
    $I0 = islt $I0, $I1
    .return ($I0)
  test_value:
    toexc = getattribute self, '$!to_exclusive'
    if toexc goto exclusive_test
    $I0 = isle topic, to
    .return ($I0)
  exclusive_test:
    $I0 = islt topic, to
    .return ($I0)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:

