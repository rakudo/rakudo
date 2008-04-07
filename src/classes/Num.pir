## $Id$

=head1 TITLE

Num - Perl 6 numbers

=cut

.namespace [ 'Num' ]

.sub 'onload' :anon :init :load
    $P0 = subclass 'Float', 'Num'
    $P1 = get_hll_global 'Any'
    $P1 = $P1.HOW()
    addparent $P0, $P1
    $P1 = get_hll_global ['Perl6Object'], 'make_proto'
    $P1($P0, 'Num')
    $P1('Float', 'Num')
.end


.sub 'ACCEPTS' :method
    .param num topic
    .return 'infix:=='(topic, self)
.end


# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
