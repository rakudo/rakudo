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
    seqproto = p6meta.'new_class'('Seq', 'parent'=>'Any', 'attr'=>'@!items $!rest')
.end


=item postcircumfix:<[ ]>(Int)

=cut

.namespace ['Seq']
.sub 'postcircumfix:<[ ]>' :method :multi(_, ['Integer'])
    .param int n
    .local pmc values, elem
    values = getattribute self, '@!items'
    elem = values[n]
    .return (elem)
.end

=head2 Private methods

=over 4

=item !STORE(source)

Performs list assignment using the values from C<source>.

=cut

.namespace ['Seq']
.sub '!STORE' :method
    .param pmc source

    # First, tell C<source> (should be a Parcel) to sequence its
    # contents.  Note that the Parcel returned by .seq is still
    # allowed to have lazy elements in it.
    source = source.'seq'()

    # Now, build a Parcel by making copies of the items from the
    # sequence where needed.  Also, watch the flatten flag on each 
    # item; if none of the items are flattening, then we have a 
    # complete sequence and won't need to iterate it.
    .local pmc parcel, item
    .local int islazy
    islazy = 0
    parcel = new ['Parcel']
  seq_loop:
    unless source goto seq_done
    item = shift source
    push parcel, item
    $P0 = getprop 'flatten', item
    if null $P0 goto seq_item
    unless $P0 goto seq_item
    islazy = 1
    push parcel, item
    goto seq_loop
  seq_item:
    $P0 = new ['ObjectRef']
    $P0.'!STORE'($P0)
    push parcel, $P0
    goto seq_loop
  seq_done:

    .local pmc rest
    null rest
    unless islazy goto have_parcel
    rest = parcel.'iterator'()
    parcel = new ['Parcel']
  have_parcel:
    setattribute self, '@!items', parcel
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
