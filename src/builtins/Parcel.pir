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
    pos_role = get_hll_global 'Positional'
    pos_role = pos_role.'!select'()
    parcelproto = p6meta.'new_class'('Parcel', 'parent'=>'parrot;ResizablePMCArray Iterable', 'does_role'=>pos_role)
.end


=item defined()

=cut

.sub 'defined' :method :vtable('defined')
    $I0 = elements self
    .tailcall '&prefix:<?>'($I0)
.end


=item item()

A Parcel in item context becomes a Seq.

=cut

.sub 'item' :method
    .tailcall self.'Seq'()
.end


=item iterator()

Construct an iterator for the Parcel.

=cut

.namespace ['Parcel']
.sub 'iterator' :method
    .local pmc listiter, rpa
    listiter = new ['List']
    rpa = new ['ResizablePMCArray']
    splice rpa, self, 0, 0
    setattribute listiter, '$!rpa', rpa
    .return (listiter)
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


=item Seq()

Return the Parcel as a Seq.

=cut

.sub 'Seq' :method
    .local pmc seq
    seq = new ['Seq']
    transform_to_p6opaque seq
    seq.'!STORE'(self)
    .return (seq)
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


=item !FETCH()

=cut

.sub '!FETCH' :method
    $P0 = self.'Seq'()
    $P0 = new ['Perl6Scalar'], $P0
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
    .local pmc eager, true
    eager = new ['Parcel']
    true = get_hll_global ['Bool'], 'True'
    setprop eager, 'rw', true

    .local pmc listiter
    listiter = new ['List']
    setattribute listiter, '$!rpa', args
  iter_loop:
    .local pmc value
    value = listiter.'get'()
    $I0 = isa value, ['EMPTY']
    if $I0 goto iter_done
    push eager, value
    goto iter_loop
  iter_done:
    .return (eager)
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

    # First, create a flattening Array from C<source>.  This
    # creates # copies of values in C<source>, in case any lvalue target
    # containers in C<self> also happen to be listed as rvalues
    # in C<source>.  (Creating a copy of everything in C<source>
    # likely isn't the most efficient approach to this, but it
    # works for now.)
    source = '&circumfix:<[ ]>'(source)

    # Now, loop through targets of C<self>, storing the corresponding
    # values from source and flattening any RPAs we encounter.  If a
    # target is an array or hash, then it will end up consuming all
    # of the remaining elements of source.
    .local pmc targets
    targets = root_new ['parrot';'ResizablePMCArray']
    splice targets, self, 0, 0
  store_loop:
    unless targets goto store_done
    .local pmc cont
    cont = shift targets
    $I0 = isa cont, ['ResizablePMCArray']
    if $I0 goto store_rpa
    $I0 = isa cont, ['Perl6Scalar']
    if $I0 goto store_scalar
    $I0 = isa cont, ['Array']
    if $I0 goto store_array
  store_scalar:
    $P0 = source.'shift'()
    cont.'!STORE'($P0)
    goto store_loop
  store_array:
    cont.'!STORE'(source)
    source = '&circumfix:<[ ]>'()
    goto store_loop
  store_rpa:
    splice targets, cont, 0, 0
    goto store_loop
  store_done:

    .return (self)
.end


=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
