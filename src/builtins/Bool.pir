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
    .local pmc p6meta, boolproto, abstraction
    p6meta = get_hll_global ['Mu'], '$!P6META'
    abstraction = get_hll_global 'Abstraction'
    boolproto = p6meta.'new_class'('Bool', 'parent'=>'parrot;Boolean Any', 'does_role'=>abstraction)

    $P0 = boolproto.'new'()
    $P0 = 0
    set_hll_global ['Bool'], 'False', $P0
    set_hll_global 'False', $P0

    $P0 = boolproto.'new'()
    $P0 = 1
    set_hll_global ['Bool'], 'True', $P0
    set_hll_global 'True', $P0
.end


.sub 'succ' :method
    $P0 = get_global 'True'
    .return ($P0)
.end


.sub 'pred' :method
    $P0 = get_global 'False'
    .return ($P0)
.end

=back

=cut


# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
