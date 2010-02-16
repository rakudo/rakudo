=head1 TITLE

List - Perl 6 List iterators

=head1 DESCRIPTION

List is used to iterate over items in a Parcel, flattening
as in list context.

=head2 Methods

=over 4

=cut

.namespace ['List']
.sub 'onload' :anon :init :load
    .local pmc p6meta, proto
    p6meta = get_hll_global ['Mu'], '$!P6META'
    proto = p6meta.'new_class'('List', 'parent'=>'Iterator', 'attr'=>'$!iter $!rpa')
.end

=item get()

Returns the next element of the list.

=cut

.sub 'get' :method
    .local pmc iter, rpa, value
    iter = getattribute self, '$!iter'
    rpa  = getattribute self, '$!rpa'

    # If we have an active subiterator, then get the next value from it.
    if null iter goto iter_done
  iter_get:
    value = iter.'get'()
    $I0 = isa value, ['EMPTY']
    unless $I0 goto get_done
    null iter
    setattribute self, '$!iter', iter
  iter_done:

    # We've exhausted the active subiterator, so now grab a value
    # from the remainder of the list.  If the list is empty, we
    # return EMPTY.
  rpa_get:
    unless rpa goto rpa_done
    value = shift rpa
    # If the value doesn't flatten, we return it directly.  
    $P0 = getprop 'scalar', value
    unless null $P0 goto get_done
    # If the value is a RPA/Parcel, it always flattens directly
    $I0 = isa value, ['ResizablePMCArray']
    if $I0 goto rpa_flatten
    # If the value is otherwise iterable, we get an active subiterator
    # and use it.
    $I0 = isa value, ['Iterable']
    unless $I0 goto get_done
    iter = value.'iterator'()
    setattribute self, '$!iter', iter
    goto iter_get
  rpa_flatten:
    splice rpa, value, 0, 0
    goto rpa_get
  rpa_done:
    value = get_hll_global 'EMPTY'

  get_done:
    .return (value)
.end
    
=back

=head2 Functions

=over 4

=item list(slurpy)

Create a list from C<slurpy>.

=cut

.namespace []
.sub '&list'
    .param pmc values          :slurpy
    .local pmc listiter
    listiter = new ['List']
    setattribute listiter, '$!rpa', values
    .return (listiter)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
