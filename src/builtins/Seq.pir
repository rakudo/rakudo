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
    seqproto = p6meta.'new_class'('Seq', 'parent'=>'Iterable', 'attr'=>'@!items $!rest')
.end

=item iterator()

Return a new SeqIterator for the invocant.

=cut

.sub 'iterator' :method
    .local pmc seqiter
    seqiter = new ['SeqIterator']
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

=head2 Private methods

=over 4

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
    $I0 = isa item, ['IterDone']
    if $I0 goto rest_done
    push items, item
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

    $P0 = getprop 'flatten', source
    if null $P0 goto source_item
    $P0 = source.'iterator'()
    source = $P0.'eager'()

  source_loop:
    unless source goto source_done
    $P0 = shift source
    $P1 = new ['ObjectRef']
    push items, $P1
    $P1.'!STORE'($P0)
    goto source_loop
  source_done:
    goto done

  source_item:
    $P1 = new ['ObjectRef']
    push items, $P1
    $P1.'!STORE'(source)

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
