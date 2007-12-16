## $Id$

=head1 NAME

src/classes/Junction.pir - Perl 6 Junction

=head1 Methods

=over 4

=cut

.namespace ['Junction']

# Constants for types of junctions.
.const int JUNCTION_TYPE_ALL  = 1
.const int JUNCTION_TYPE_ANY  = 2
.const int JUNCTION_TYPE_ONE  = 3
.const int JUNCTION_TYPE_NONE = 4

.sub 'onload' :anon :load :init
    $P0 = subclass 'Perl6Object', 'Junction'
    addattribute $P0, "@values"
    addattribute $P0, "$type"
    $P1 = get_hll_global ['Perl6Object'], 'make_proto'
    $P1($P0, 'Junction')
.end


=item values()

Get the values in the junction.

=cut

.sub 'values' :method
    $P0 = getattribute self, "@values"
    $P0 = clone $P0
    .return($P0)
.end


=item !values(...)

Private method to sets the values in the junction.

=cut

.sub '!values' :method
    .param pmc list
    setattribute self, "@values", list
.end


=item !type(...)

Private method to set the type of the junction.

=cut

.sub '!type' :method
    .param pmc type
    setattribute self, "$type", type
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
