## $Id$

=head1 NAME

src/classes/List.pir - Perl 6 List class

=head1 Functions

=over 4

=cut

.namespace

.sub '__onload' :load :init
    $P0 = get_hll_global ['Perl6Object'], 'make_class'
    $P0('List', 'super'=>'ResizablePMCArray')
.end

.namespace ['List']

.sub 'get_string' :vtable :method
    $S0 = join ' ', self
    .return ($S0)
.end

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
