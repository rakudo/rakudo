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
    $P0 = subclass 'Boolean', 'Bool'
    $P1 = get_hll_global 'Any'
    $P1 = $P1.HOW()
    addparent $P0, $P1
    $P1 = get_hll_global ['Perl6Object'], 'make_proto'
    protoobject = $P1($P0, 'Bool')

    $P0 = protoobject.'new'()
    $P0 = 0
    set_hll_global [ 'Bool' ], 'False', $P0

    $P0 = protoobject.'new'()
    $P0 = 1
    set_hll_global [ 'Bool' ], 'True', $P0
.end


.sub 'ACCEPTS' :method
    .param pmc topic
    .return (self)
.end

.sub 'increment' :method :vtable
    self = 1
.end

.sub 'decrement' :method :vtable
    self = 0
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
