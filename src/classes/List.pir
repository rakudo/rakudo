## $Id$

=head1 NAME

src/classes/List.pir - Perl 6 List class

=head1 Methods

=over 4

=cut

.namespace ['List']

.sub 'onload' :anon :load :init
    $P0 = subclass 'ResizablePMCArray', 'List'
    $P1 = get_hll_global ['Perl6Object'], 'make_proto'
    $P1($P0, 'List')
.end


=item get_string()    (vtable method)

Return the elements of the list joined by spaces.

=cut

.sub 'get_string' :vtable :method
    $S0 = join ' ', self
    .return ($S0)
.end


=item elems()

Return the number of elements in the list.

=cut

.sub 'elems' :method
    $I0 = elements self
    .return ($I0)
.end


.sub 'unshift' :method
    .param pmc x
    unshift self, x
.end


.sub 'shift' :method
    .local pmc x
    x = shift self
    .return (x)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
