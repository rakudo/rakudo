## $Id$

=head1 TITLE

Bool - Perl 6 boolean class

=head1 DESCRIPTION

This file sets up the Perl 6 C<Order> class, and initializes
symbols for C<Order::Increase>, C<Order::Decrease>, and C<Order::Same>.

Note that one we have true Perl 6 enums this file will probably
disappear and the definition moved into a prelude.

=cut

.namespace ['Order']

.sub 'onload' :anon :init :load
    .local pmc p6meta, orderproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    orderproto = p6meta.'new_class'('Order', 'parent'=>'Int')

    $P0 = orderproto.'new'()
    $P0 = 0
    set_hll_global ['Order'], 'Same', $P0

    $P0 = orderproto.'new'()
    $P0 = 1
    set_hll_global ['Order'], 'Decrease', $P0

    $P0 = orderproto.'new'()
    $P0 = -1
    set_hll_global ['Order'], 'Increase', $P0

    # Mark as enum elements.
    $P0 = class $P0
    $P1 = box 1
    setprop $P0, 'enum', $P1
.end


.sub 'perl' :method
    if self < 0 goto increase
    if self > 0 goto decrease
    .return ('Order::Same')
  increase:
    .return ('Order::Increase')
  decrease:
    .return ('Order::Decrease')
.end


# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
