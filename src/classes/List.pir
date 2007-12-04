## $Id$

=head1 NAME

src/classes/List.pir - Perl 6 List class

=head1 Functions

=over 4

=cut

.namespace

.sub '__onload' :load :init
    $P0 = subclass 'ResizablePMCArray', 'List'
    $P1 = get_class 'Perl6Object'
    $P0.add_parent($P1)

    $P1 = new $P0
    set_hll_global 'List', $P1
.end

.namespace ['List']

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
