=head1

Return an iterator that iterates over a Class' attributes.
If the Class object has a @!attribute_list property, use
that as the order of attributes, otherwise introspect the
class and use its list.  (As of Parrot 1.4.0 we can't
always introspect the class directly, as the order of
attributes in the class isn't guaranteed.)

=cut

.HLL 'parrot'

.namespace ['Class']
.sub 'attriter' :method
    $P0 = getprop '@!attribute_list', self
    unless null $P0 goto have_list
    $P0 = inspect self, 'attributes'
  have_list:
    $P1 = iter $P0
    .return ($P1)
.end


