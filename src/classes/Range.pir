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

=item ACCEPTS(topic)

Determines if topic is within the range.

=cut

.sub 'ACCEPTS' :method
    .param pmc topic

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


=item shift()   (vtable_method)

Generate the next element at the front of the Range.

=cut

.sub 'shift' :method :vtable('shift_pmc')
    .local pmc from, fromexc, value
    from = getattribute self, '$!from'
    fromexc = getattribute self, '$!from_exclusive'
    value = 'postfix:++'(from)
    unless fromexc goto have_value
    value = from
  have_value:
    $I0 = self.'!to_test'(value)
    if $I0 goto success
    value = new 'Failure'
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
    .return proto.'new'('from'=>from, 'to'=>to)
.end

.sub 'infix:^..'
    .param pmc from
    .param pmc to
    .local pmc proto, true
    proto = get_hll_global 'Range'
    true = get_hll_global ['Bool'], 'True'
    .return proto.'new'('from'=>from, 'to'=>to, 'from_exclusive'=>true)
.end

.sub 'infix:..^'
    .param pmc from
    .param pmc to
    .local pmc proto, true
    proto = get_hll_global 'Range'
    true = get_hll_global ['Bool'], 'True'
    .return proto.'new'('from'=>from, 'to'=>to, 'to_exclusive'=>true)
.end

.sub 'infix:^..^'
    .param pmc from
    .param pmc to
    .local pmc proto, true
    proto = get_hll_global 'Range'
    true = get_hll_global ['Bool'], 'True'
    .return proto.'new'('from'=>from, 'to'=>to, 'from_exclusive'=>true, 'to_exclusive'=>true)
.end



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
    toexc = getattribute self, '$!to_exclusive'
    if toexc goto exclusive_test
    $I0 = isle topic, to
    .return ($I0)
  exclusive_test:
    $I0 = islt topic, to
    .return ($I0)
.end


