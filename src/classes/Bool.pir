## $Id$

=head1 TITLE

Bool - Perl 6 boolean class

=head1 DESCRIPTION

This file sets up the Perl 6 C<Bool> class, and initializes
symbols for C<Bool::True> and C<Bool::False>.

=cut

.namespace ['Bool']

.sub 'onload' :anon :init :load
    .local pmc protoobject
    $P0 = get_hll_global ['Perl6Object'], 'make_proto'
#    protoobject = $P0('Boolean', 'Bool')
    protoobject = $P0('Perl6Bool', 'Bool')

    $P0 = protoobject.'new'()
    $P0 = 0
    set_global 'False', $P0

    $P0 = protoobject.'new'()
    $P0 = 1
    set_global 'True', $P0
.end


.sub 'ACCEPTS' :method
    .param pmc topic
    .return (self)
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
