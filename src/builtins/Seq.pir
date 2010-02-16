=head1 TITLE

Seq - Perl 6 Seq class

=head1 DESCRIPTION

This module implements Seq, an immutable (but potentially lazy)
sequence of items.

=head2 Methods

=over 4

=cut

.namespace ['Seq']
.sub 'onload' :anon :init :load
    .local pmc p6meta, seqproto, pos_role
    p6meta = get_hll_global ['Mu'], '$!P6META'
    
    # Select generic version of Positional (for untyped) and do it.
    # XXX When List becomes a parametric role too, we'd pass in the
    # given T to select below.
    pos_role = get_hll_global 'Positional'
    pos_role = pos_role.'!select'()

    # Create the class.
    seqproto = p6meta.'new_class'('Seq', 'parent'=>'Iterable', 'attr'=>'@!items $!rest', 'does_role'=>pos_role)
.end

=item iterator()

Return a new SeqIter for the invocant.

=cut

.sub 'iterator' :method
    .local pmc seqiter
    seqiter = new ['SeqIter']
    setattribute seqiter, '$!seq', self
    $P0 = box 0
    setattribute seqiter, '$!index', $P0
    .return (seqiter)
.end


=item postcircumfix:<[ ]>(Int)

=cut

.namespace ['Seq']
.sub 'postcircumfix:<[ ]>' :method :multi(_, ['Integer'])
    .param int n
    .local pmc items, elem
    items = getattribute self, '@!items'
    $I0 = elements items
    if n < $I0 goto have_items
    $I0 = n + 1
    items = self.'!fill'($I0)
  have_items:
    elem = items[n]
    .return (elem)
.end


=item Capture

Coerces the Seq to a Capture. For now, not lazy.

=cut

.sub 'Capture' :method
    $P0 = self.'!fill'()
    $P1 = get_hll_global 'Capture'
    $P1 = $P1.'new'($P0 :flat)
    .return ($P1)
.end


=head2 Private methods

=over 4

=item !elem(item)

Create a new empty element in the Seq, initialize it to item.
This method is overridden by subclasses (e.g., Array) that
wish to make other property changes on individual elements.

=cut

.namespace ['Seq']
.sub '!elem' :method
    .param pmc item
    .local pmc elem, true
    item = descalarref item
    elem = new ['ObjectRef'], item
    true = get_hll_global ['Bool'], 'True'
    setprop elem, 'scalar', true
    .return (elem)
.end

=item !fill([n])

Reify any lazy portions of the sequence until we have at least
C<n> items.  If C<n> is omitted, then reify the entire sequence.

=cut

.sub '!fill' :method
    .param int n               :optional
    .param int has_n           :opt_flag

    .local pmc items, rest
    items = getattribute self, '@!items'
    rest  = getattribute self, '$!rest'

    unless null items goto have_items
    items = root_new ['parrot';'ResizablePMCArray']
    setattribute self, '@!items', items
  have_items:

    # If there's no $!rest, then we can stop immediately, as
    # everything has already been reified.
    if null rest goto done

    .local int items_n
    items_n = elements items
  rest_loop:
    unless has_n goto rest_1
    if n <= items_n goto done
  rest_1:
    .local pmc item
    item = rest.'get'()
    $I0 = isa item, ['EMPTY']
    if $I0 goto rest_done
    $P0 = self.'!elem'(item)
    push items, $P0
    inc items_n
    goto rest_loop
  rest_done:
    null rest
    setattribute self, '$!rest', rest

  done:
    .return (items)
.end
    

=item !STORE(source)

Performs list assignment using the values from C<source>.

=cut

.namespace ['Seq']
.sub '!STORE' :method
    .param pmc source

    .local pmc items, rest
    items = new ['Parcel']
    null rest

    $P0 = getprop 'scalar', source
    unless null $P0 goto source_item
    $I0 = isa source, ['Iterable']
    unless $I0 goto source_item
    $P0 = source.'iterator'()
    source = $P0.'eager'()

  source_loop:
    unless source goto source_done
    $P0 = shift source
    $P1 = self.'!elem'($P0)
    push items, $P1
    goto source_loop
  source_done:
    goto done

  source_item:
    $P1 = self.'!elem'(source)
    push items, $P1

  done:
    setattribute self, '@!items', items
    setattribute self, '$!rest', rest
    .return (self)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
