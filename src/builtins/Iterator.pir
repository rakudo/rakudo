=head1 TITLE

Iterator - Perl 6 Iterator (abstract) class

=head1 DESCRIPTION

Iterator is the base class for creating iterators.
(Currently I've defined it as a class; eventually it
may be a role.)  Subclasses are required to override
the .get method; other methods may also be overridden.

=head2 Methods

=over 4

=cut

.namespace ['Iterator']
.sub 'onload' :anon :init :load
    .local pmc p6meta, proto
    p6meta = get_hll_global ['Mu'], '$!P6META'
    proto = p6meta.'new_class'('Iterator', 'parent'=>'Any')
.end

=item eager()

Returns a Parcel containing all of the remaining items
in the Iteration.  If the invocant is infinite, then
continue until memory is exhausted.

=cut

.namespace ['Iterator']
.sub 'eager' :method
    .local pmc parcel, true
    parcel = new ['Parcel']
    true = get_hll_global ['Bool'], 'True'
    setprop parcel, 'flatten', true
  iter_loop:
    .local pmc item
    item = self.'get'()
    $I0 = isa item, ['IterDone']
    if $I0 goto iter_done
    push parcel, item
    goto iter_loop
  iter_done:
    .return (parcel)
.end


=item seq()

Return the invocant in a form suitable for use in a Seq object.
The default is to eagerly evaluate the invocant; subclasses
can override this default to handle infinite or other lazily
evaluated series.

=cut

.namespace ['Iterator']
.sub 'seq' :method
    .tailcall self.'eager'()
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
