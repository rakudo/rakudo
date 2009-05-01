## $Id$

=head1 TITLE

Bool - Perl 6 boolean class

=head1 DESCRIPTION

This file sets up the Perl 6 C<Bool> class, and initializes
symbols for C<Bool::True> and C<Bool::False>.

=head1 Methods

=over 4

=cut

.namespace ['Bool']

.sub 'onload' :anon :init :load
    .local pmc p6meta, boolproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    boolproto = p6meta.'new_class'('Bool', 'parent'=>'parrot;Boolean Any')
    boolproto.'!IMMUTABLE'()
    p6meta.'register'('Boolean', 'parent'=>boolproto, 'protoobject'=>boolproto)

    # True and False need to appear type-ish.
    $P1 = get_hll_global 'Abstraction'

    $P0 = boolproto.'new'()
    $P0 = 0
    'infix:does'($P0, $P1)
    set_hll_global ['Bool'], 'False', $P0
    set_hll_global 'False', $P0

    $P0 = boolproto.'new'()
    $P0 = 1
    'infix:does'($P0, $P1)
    set_hll_global ['Bool'], 'True', $P0
    set_hll_global 'True', $P0
.end


.sub 'succ' :method :vtable('increment')
    self = 1
.end


.sub 'pred' :method :vtable('decrement')
    self = 0
.end

=back

=cut


# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
