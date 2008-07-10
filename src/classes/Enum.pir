## $Id:$

=head1 TITLE

Enum - Perl 6 Enum base class

=head1 DESCRIPTION

This file sets up the C<Enum> class, which is the base class for all
enumeration types.

=cut

.namespace ['Enum']

.sub 'onload' :anon :init :load
    .local pmc p6meta
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    p6meta.'new_class'('Enum', 'parent'=>'Any')
.end

=head2 Methods

=over 4

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
