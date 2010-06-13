=head1 TITLE

Iterator - Perl 6 Iterator (abstract) class

=head1 DESCRIPTION

Iterator is the base class for creating iterators.
(Currently I've defined it as a class; eventually it
may be a role.)  Subclasses are required to override
the .get method; other methods may also be overridden.

Conjecturally, Iterators are also Iterable -- i.e., they
flatten in list context.

=head2 Methods

=over 4

=cut

.namespace ['Iterator']
.sub 'onload' :anon :init :load
    .local pmc p6meta, proto, pos_role
    p6meta = get_hll_global ['Mu'], '$!P6META'
    proto = p6meta.'new_class'('Iterator', 'parent'=>'Iterable')
.end


.namespace ['Iterator']
.sub 'list' :method
    .local pmc list
    list = new ['List']
    $P0 = root_new ['parrot';'ResizablePMCArray']
    $P1 = descalarref self
    push $P0, $P1
    setattribute list, '@!rest', $P0
    .return (list)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
