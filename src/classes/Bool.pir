## $Id$

=head1 TITLE

Bool - Perl 6 boolean class

=head1 DESCRIPTION

This file sets up the Perl 6 C<Bool> class, and initializes
symbols for C<Bool::True> and C<Bool::False>.

=cut

.namespace ['Bool']

.sub 'onload' :anon :init :load
    .local pmc p6meta, boolproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    boolproto = p6meta.'new_class'('Bool', 'parent'=>'Boolean Any')
    # p6meta.'register'('Boolean', 'parent'=>'Any', 'protoobject'=>boolproto)

    $P0 = boolproto.'new'()
    $P0 = 0
    set_hll_global ['Bool'], 'False', $P0

    $P0 = boolproto.'new'()
    $P0 = 1
    set_hll_global ['Bool'], 'True', $P0

    # 'our bit enum bool <True False>'
    $P0 = new 'Integer'
    $P0 = 0
    set_hll_global 'False', $P0
    $P0 = new 'Integer'
    $P0 = 1
    set_hll_global 'True', $P0
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
