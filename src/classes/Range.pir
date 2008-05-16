## $Id:$

=head1 NAME

src/classes/Range.pir - methods for the Range class

=head1 Methods

=over 4

=cut

.namespace ['Range']

.sub 'onload' :anon :load :init
    $P0 = subclass 'Any', 'Range'
    addattribute $P0, "$!from"
    addattribute $P0, "$!to"
    $P1 = get_hll_global ['Perl6Object'], 'make_proto'
    $P1($P0, 'Range')
.end


=item clone (vtable method)

Ranges are immutable, so just return ourself.

=cut

.sub 'clone' :method :vtable
    .return (self)
.end


=item from

Gets the value the range starts from.

=cut

.sub 'from' :method
    $P0 = getattribute self, '$!from'
    .return ($P0)
.end


=item to

Gets the value the range goes up to.

=cut

.sub 'to' :method
    $P0 = getattribute self, '$!to'
    .return ($P0)
.end


=item perl

Returns a Perl code representation of the range.

=cut

.sub perl :method
    # Get to and from, and then their perl representations.
    $P0 = getattribute self, '$!from'
    $S0 = $P0.'perl'()
    $P1 = getattribute self, '$!to'
    $S1 = $P1.'perl'()

    # Generate to...from
    concat $S0, ".."
    concat $S0, $S1
    .return($S0)
.end


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
