## $Id: List.pir 23848 2007-12-13 14:26:02Z pmichaud $

=head1 NAME

src/classes/Junction.pir - Perl 6 Junction

=head1 Methods

=over 4

=cut

.namespace ['Junction']

.include "src/builtins/junctiontypes.pir"

.sub 'onload' :anon :load :init
    $P0 = newclass 'Junction'
    addattribute $P0, "values"
    addattribute $P0, "type"
    $P1 = get_hll_global ['Perl6Object'], 'make_proto'
    $P1($P0, 'Junction')
.end

=item values()

Get the values in the junction.

=cut

.sub 'values' :method
    $P0 = getattribute self, "values"
    $P0 = clone $P0
    .return($P0)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
