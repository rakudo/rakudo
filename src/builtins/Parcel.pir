=head1 TITLE

Parcel - Perl 6 Parcel class

=head1 DESCRIPTION

This file implements Parcels, which holds a sequence of
elements and can be flattened into Captures or Lists.

=head2 Methods

=over 4

=cut

.namespace ['Parcel']
.sub 'onload' :anon :init :load
    .local pmc p6meta, parcelproto, pos_role
    p6meta = get_hll_global ['Mu'], '$!P6META'
    parcelproto = p6meta.'new_class'('Parcel', 'parent'=>'parrot;ResizablePMCArray Iterable')
.end

.sub '' :vtable('get_string') :method
    $S0 = self.'Str'()
    .return ($S0)
.end


=item item()

A Parcel in item context becomes a Seq.

=cut

.sub 'item' :method
    .local pmc seq, flat, rest
    seq = get_hll_global 'Seq'
    seq = seq.'new'(self)
    seq.'eager'()
    .return (seq)
.end


=item hash()

=cut

.sub 'hash' :method
    .tailcall 'hash'(self)
.end


=item iterator()

Construct an iterator for the Parcel.

=cut

.namespace ['Parcel']
.sub 'iterator' :method
    .local pmc parceliter, rpa
    parceliter = new ['ParcelIter']
    rpa = root_new ['parrot';'ResizablePMCArray']
    splice rpa, self, 0, 0
    setattribute parceliter, '$!parcel', rpa
    .return (parceliter)
.end


=item perl

=cut

.namespace ['Parcel']
.sub 'perl' :method
    .local pmc self_it, perllist
    self_it = iter self
    perllist = new ['ResizablePMCArray']
  self_loop:
    unless self_it goto self_done
    $P0 = shift self_it
    $S0 = $P0.'perl'()
    push perllist, $S0
    goto self_loop
  self_done:
    $I0 = elements perllist
    unless $I0 == 1 goto have_perllist
    push perllist, ''
  have_perllist:
    $S0 = join ', ', perllist
    $S0 = concat '(', $S0
    $S0 = concat $S0, ')'
    .return ($S0)
.end


=item Bool()

=cut

.sub 'Bool' :method
    $I0 = istrue self
    .tailcall '&prefix:<?>'($I0)
.end


=item Capture()

Coerce the Parcel into a capture.

=cut

.namespace ['Parcel']
.sub 'Capture' :method
    .local pmc self_it, pos, named
    self_it = iter self
    pos = root_new ['parrot';'ResizablePMCArray']
    named = root_new ['parrot';'Hash']
  self_loop:
    unless self_it goto self_done
    $P0 = shift self_it
    $I0 = isa $P0, 'Enum'
    if $I0 goto to_named
    push pos, $P0
    goto self_loop
  to_named:
    $P1 = $P0.'key'()
    $P2 = $P0.'value'()
    named[$P1] = $P2
    goto self_loop
  self_done:
    $P0 = get_hll_global 'Capture'
    $P0 = $P0.'new'(pos :flat, named :flat :named)
    .return ($P0)
.end


=back

=head2 Functions

=over 4

=item &eager

Return a Parcel containing the eager evaluation of all items
in a list.

=cut

.namespace []
.sub '&eager'
    .param pmc args            :slurpy
    .local pmc parcel
    parcel = new ['Parcel']
    splice parcel, args, 0, 0
    $P0 = parcel.'flat'()
    $P0 = $P0.'eager'()
    .return ($P0)
.end


=item &Nil

The canonical function for creating an empty Parcel.

=cut

.namespace []
.sub '&Nil'
    .param pmc args            :slurpy
    $P0 = '&infix:<,>'()
    .return ($P0)
.end


=back

=head2 Operators

=over 4

=item &infix:<,>

The canonical operator for creating a Parcel.

=cut

.namespace []
.sub '&infix:<,>'
    .param pmc args            :slurpy
    # Recast the arguments into a Parcel object
    .local pmc parcel
    parcel = new ['Parcel']
    transform_to_p6opaque parcel
    splice parcel, args, 0, 0
    # treat parcel itself as rw (for list assignment)
    $P0 = get_hll_global ['Bool'], 'True'
    setprop parcel, 'rw', $P0
    .return (parcel)
.end

=back

=head2 Private methods

=over 4

=item !STORE(source)

Handle assignment to a Parcel (list assignment).

=cut

.namespace ['Parcel']
.sub '!STORE' :method
    .param pmc source

    # We do this in two passes.  The first pass creates
    # two RPAs; one is a flattened RPA of LHS target containers
    # the other is the de-containerized RHS values.  This
    # first pass avoids the potential problem of a LHS container
    # appearing on the RHS, and it also preserves laziness for
    # assignment to scalars.
    # The second pass then simply assigns the collected values
    # to the collected targets.

    .local pmc lhs, tv, seq
    lhs = root_new ['parrot';'ResizablePMCArray']
    tv  = root_new ['parrot';'ResizablePMCArray']
    splice lhs, self, 0, 0

    $P0 = get_hll_global 'Seq'
    seq = $P0.'new'(source)

    # Walk through the lhs, building targets and values as we go.
  lhs_loop:
    unless lhs goto lhs_done
    .local pmc cont
    cont = shift lhs
    $I0 = isa cont, ['ResizablePMCArray']
    if $I0 goto cont_rpa
    unless seq goto cont_nil
    $I0 = isa cont, ['Whatever']
    if $I0 goto cont_scalar
    $P0 = getprop 'scalar', cont
    if null $P0 goto cont_array
    unless $P0 goto cont_array
  cont_scalar:
    $P0 = seq.'shift'()
    push tv, cont
    push tv, $P0
    goto lhs_loop
  cont_array:
    seq.'eager'()
    push tv, cont
    push tv, seq
    seq = get_hll_global 'False'
    goto lhs_loop
  cont_nil:
    $P0 = '&Nil'()
    push tv, cont
    push tv, $P0
    goto lhs_loop
  cont_rpa:
    splice lhs, cont, 0, 0
    goto lhs_loop
  lhs_done:

  # Okay, tv is now target+value pairs, so
  # we loop through it, performing the assignment
  # on each pair.
  tv_loop:
    unless tv goto tv_done
    $P0 = shift tv
    $P1 = shift tv
    '&infix:<=>'($P0, $P1)
    goto tv_loop
  tv_done:
    .return (self)
.end


=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
