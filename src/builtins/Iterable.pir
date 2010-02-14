
=head1 TITLE

Iterable - Perl 6 Iterable Role

=head1 DESCRIPTION

Iterable is the role for objects that can produce
iterators and flatten in list contexts.

XXX:  This file implements Iterable as a class
for the time being, because Pm was tired of struggling
with making it into a role.  Eventually it should
be made into a role, and the classes that are defined
as "is Iterable" should be come "does Iterable".

=head2 Methods

=over 4

=cut

.namespace ['Iterable']
.sub 'onload' :anon :init :load
    .local pmc p6meta, proto
    p6meta = get_hll_global ['Mu'], '$!P6META'
    proto = p6meta.'new_class'('Iterable', 'parent'=>'Any')
.end

=item list()

=cut

.sub 'list' :method
    $P0 = descalarref self
    .return ($P0)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
