## $Id$

=head1 NAME

src/classes/Scalar.pir - Perl 6 Array class and related functions

=head2 Object Methods

=cut

.namespace []

#.sub 'onload' :anon :load :init
#    .local pmc p6meta, scalarproto
#    $P0 = subclass 'Mutable', 'Perl6Scalar'
#.end

.namespace ['Perl6Scalar']
.sub 'infix:=' :method
    .param pmc source
    $P0 = source.'item'()
    assign self, $P0
    .return (self)
.end


# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:

